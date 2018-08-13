;;;; opencl-tests.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(in-package #:opencl-tests)

(defun show-devices ()
  (dolist (platform (eazy-opencl.host:get-platform-ids))
    (dolist (dev-id (eazy-opencl.host:get-device-ids platform :device-type-all))
      (format t "================================~%Name: ~a~%    Type ~a~%~%"
              (eazy-opencl.host:get-device-info dev-id :device-name)
              (eazy-opencl.host:get-device-info dev-id :device-type)))))

(defmacro with-err-handler (&body body)
  (with-gensyms (cval) 
    `(handler-case
         (progn
           ,@body)
       (%ocl:opencl-error (,cval)
         (format t "OpenCL Error: ~s~%" (%ocl:opencl-error-code ,cval))))))

(defun mandelbrot-set (width height)
  (let* ((platform (car (eazy-opencl.host:get-platform-ids)))
         (devices (list (car (eazy-opencl.host:get-device-ids platform :device-type-gpu))))
         (context (eazy-opencl.host:create-context devices :context-platform platform))
         (out-device (eazy-opencl.host:create-buffer context '(:mem-write-only) (* width height)))
         (compiled-program (eazy-opencl.host:create-program-with-source
                            context
                            (alexandria:read-file-into-string "~/src/lisp/opencl-tests/mandelbrot.cl")))
         (built-program (if-let ((program (with-err-handler (eazy-opencl.host:build-program compiled-program :devices devices))))
                          program
                          (progn 
                            (format t "BPF: ~s~%" 
                                    (eazy-opencl.host:get-program-build-info
                                     compiled-program
                                     (car devices)
                                     :program-build-log))
                            program)))
         (kernel (eazy-opencl.host:create-kernel built-program "mandelbrot")))
    (format t "Compiled mandelbrot.cl: ~a~%" built-program)
    (list platform devices context out-device compiled-program built-program kernel )))
