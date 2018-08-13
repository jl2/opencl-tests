;;;; opencl-tests.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(in-package #:opencl-tests)

(defun show-devices ()
  (dolist (thing '(:device-name
                   :device-type
                   :device-extensions
                   :device-execution-capabilities
                   :DEVICE-ADDRESS-BITS
                   :DEVICE-AVAILABLE
                   :DEVICE-BUILT-IN-KERNELS
                   :DEVICE-COMPILER-AVAILABLE
                   :DEVICE-DOUBLE-FP-CONFIG :DEVICE-ENDIAN-LITTLE
                   :DEVICE-ERROR-CORRECTION-SUPPORT
                   :DEVICE-EXECUTION-CAPABILITIES
                   :DEVICE-GLOBAL-MEM-CACHE-SIZE
                   :DEVICE-GLOBAL-MEM-CACHE-TYPE
                   :DEVICE-GLOBAL-MEM-CACHELINE-SIZE
                   :DEVICE-GLOBAL-MEM-SIZE
                   :DEVICE-HOST-UNIFIED-MEMORY
                   :DEVICE-IMAGE-BASE-ADDRESS-ALIGNMENT
                   :DEVICE-IMAGE-MAX-ARRAY-SIZE
                   :DEVICE-IMAGE-MAX-BUFFER-SIZE
                   :DEVICE-IMAGE-PITCH-ALIGNMENT
                   :DEVICE-IMAGE-SUPPORT :DEVICE-IMAGE2D-MAX-HEIGHT
                   :DEVICE-IMAGE2D-MAX-WIDTH
                   :DEVICE-IMAGE3D-MAX-DEPTH
                   :DEVICE-IMAGE3D-MAX-HEIGHT
                   :DEVICE-IMAGE3D-MAX-WIDTH
                   :DEVICE-LINKER-AVAILABLE :DEVICE-LOCAL-MEM-SIZE
                   :DEVICE-LOCAL-MEM-TYPE
                   :DEVICE-MAX-CLOCK-FREQUENCY
                   :DEVICE-MAX-COMPUTE-UNITS
                   :DEVICE-MAX-CONSTANT-ARGS
                   :DEVICE-MAX-CONSTANT-BUFFER-SIZE
                   :DEVICE-MAX-MEM-ALLOC-SIZE
                   :DEVICE-MAX-PARAMETER-SIZE
                   :DEVICE-MAX-READ-IMAGE-ARGS :DEVICE-MAX-SAMPLERS
                   :DEVICE-MAX-WORK-GROUP-SIZE
                   :DEVICE-MAX-WORK-ITEM-DIMENSIONS
                   :DEVICE-MAX-WORK-ITEM-SIZES
                   :DEVICE-MAX-WRITE-IMAGE-ARGS
                   :DEVICE-MEM-BASE-ADDR-ALIGN
                   :DEVICE-MIN-DATA-TYPE-ALIGN-SIZE :DEVICE-NAME
                   :DEVICE-NATIVE-VECTOR-WIDTH-CHAR
                   :DEVICE-NATIVE-VECTOR-WIDTH-DOUBLE
                   :DEVICE-NATIVE-VECTOR-WIDTH-FLOAT
                   :DEVICE-NATIVE-VECTOR-WIDTH-HALF
                   :DEVICE-NATIVE-VECTOR-WIDTH-INT
                   :DEVICE-NATIVE-VECTOR-WIDTH-LONG
                   :DEVICE-NATIVE-VECTOR-WIDTH-SHORT
                   :DEVICE-OPENCL-C-VERSION :DEVICE-PARENT-DEVICE
                   :DEVICE-PARTITION-AFFINITY-DOMAIN
                   :DEVICE-PARTITION-MAX-SUB-DEVICES
                   :DEVICE-PARTITION-PROPERTIES
                   :DEVICE-PARTITION-TYPE :DEVICE-PLATFORM
                   :DEVICE-PREFERRED-INTEROP-USER-SYNC
                   :DEVICE-PREFERRED-VECTOR-WIDTH-CHAR
                   :DEVICE-PREFERRED-VECTOR-WIDTH-DOUBLE
                   :DEVICE-PREFERRED-VECTOR-WIDTH-FLOAT
                   :DEVICE-PREFERRED-VECTOR-WIDTH-HALF
                   :DEVICE-PREFERRED-VECTOR-WIDTH-INT
                   :DEVICE-PREFERRED-VECTOR-WIDTH-LONG
                   :DEVICE-PREFERRED-VECTOR-WIDTH-SHORT
                   :DEVICE-PRINTF-BUFFER-SIZE :DEVICE-PROFILE
                   :DEVICE-PROFILING-TIMER-RESOLUTION
                   :DEVICE-QUEUE-PROPERTIES :DEVICE-REFERENCE-COUNT
                   :DEVICE-SINGLE-FP-CONFIG :DEVICE-TYPE
                   :DEVICE-VENDOR :DEVICE-VENDOR-ID :DEVICE-VERSION
                   :DRIVER-VERSION))
    (format t "~%~48a " thing)
    (dolist (platform (eazy-opencl.host:get-platform-ids))
      (dolist (dev-id (eazy-opencl.host:get-device-ids platform :device-type-all))
        (let ((val (eazy-opencl.host:get-device-info dev-id thing)))
          (format t "~48a " val))))))

(defmacro with-err-handler (&body body)
  (with-gensyms (cval) 
    `(handler-case
         (progn
           ,@body)
       (%ocl:opencl-error (,cval)
         (format t "OpenCL Error: ~s~%" (%ocl:opencl-error-code ,cval))))))

(defun mandelbrot-set (width height)
  (eazy-opencl.fancy:with-easy-opencl-setup (_
                                            (device (lambda (device)
                                                      (and 
                                                       (string= (eazy-opencl.host:get-device-info device :device-vendor) "NVIDIA")
                                                       (member :device-type-gpu (eazy-opencl.host:get-device-info device :device-type)))))
                                            context
                                            queue)

    
    (let* ((out-buffer (cffi:lisp-array-to-foreign ((make-array (* width height) :element-type single-float :initial-element 0.0f0) out-buffer :double*)))
           (out-device (eazy-opencl.host:create-buffer context '(:mem-write-only :mem-use-host-ptr) size out-buffer))
           (compiled-program (eazy-opencl.host:create-program-with-source
                                context
                                (alexandria:read-file-into-string "~/src/lisp/opencl-tests/mandelbrot.cl")))
             (built-program (if-let
                                ((program (with-err-handler
                                            (eazy-opencl.host:build-program compiled-program
                                                                            :devices (list device)))))
                              program
                              (progn 
                                (format t "build-program error: ~s~%" 
                                        (eazy-opencl.host:get-program-build-info
                                         compiled-program
                                         device
                                         :program-build-log))
                                program)))
             (kernel (eazy-opencl.host:create-kernel built-program "mandelbrot")))
      
        (eazy-opencl.host:set-kernel-arg kernel 0 out-device '%ocl:mem)
        (eazy-opencl.host:set-kernel-arg kernel 1 width :int)
        (eazy-opencl.host:set-kernel-arg kernel 2 height :int)
        (%ocl/h::with-foreign-array (global-work-size '%ocl:size-t (list size))
          (%ocl:enqueue-nd-range-kernel queue kernel 1 (cffi:null-pointer) global-work-size (cffi:null-pointer) 0 (cffi:null-pointer) (cffi:null-pointer)))

        (%ocl:enqueue-read-buffer queue out-device %ocl:true 0 size out-buffer 0 (cffi:null-pointer) (cffi:null-pointer))

        (format t "Compiled mandelbrot.cl: ~a~%" built-program)))))

(defun test-easy-setup ()
  (eazy-opencl.fancy:with-easy-opencl-setup (_
                                            (device (lambda (device)
                                                      (and 
                                                       (string= (eazy-opencl.host:get-device-info device :device-vendor) "NVIDIA")
                                                       (member :device-type-gpu (eazy-opencl.host:get-device-info device :device-type)))))
                                            ctx
                                            queue)
    (format t "Hello world from opencl: ~a.~%"
            (cffi:with-foreign-pointer-as-string ((out-host size) 13) ;; Hello, World<null> : char[13]
              (let* ((out-device (eazy-opencl.host:create-buffer ctx '(:mem-write-only :mem-use-host-ptr) size out-host))
                     (program    (eazy-opencl.host:create-program-with-source ctx (alexandria:read-file-into-string "~/src/lisp/opencl-tests/helloworld.cl"))))
                (eazy-opencl.host:build-program program :devices (list device))
                (let ((kernel (eazy-opencl.host:create-kernel program "hello")))
                  (eazy-opencl.host:set-kernel-arg kernel 0 out-device '%ocl:mem)
                  (%ocl/h::with-foreign-array (global-work-size '%ocl:size-t (list size))
                    (%ocl:enqueue-nd-range-kernel queue kernel 1 (cffi:null-pointer) global-work-size (cffi:null-pointer) 0 (cffi:null-pointer) (cffi:null-pointer)))
                  (%ocl:enqueue-read-buffer queue out-device %ocl:true 0 size out-host 0 (cffi:null-pointer) (cffi:null-pointer))))))))
