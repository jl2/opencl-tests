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

(defun mandelbrot-set (width height)
  (let* ((platform (car (eazy-opencl.host:get-platform-ids)))
         (devices (eazy-opencl.host:get-device-ids platform :device-type-cpu))
         (context (eazy-opencl.host:create-context devices ))
         (out-device (eazy-opencl.host:create-buffer context '(:mem-write-only :mem-use-host-ptr) (* width height)))
         (compiled-program (eazy-opencl.host:create-program-with-source
                            context
                            (alexandria:read-file-into-string "~/src/lisp/opencl-tests/mandelbrot.cl")))
         (built-program (eazy-opencl.host:build-program compiled-program :devices devices))
         (kernel (eazy-opencl.host:create-kernel built-program "mandel")))
    (format t "Compiled mandelbrot.cl: ~a~%" built-program)
    (list platform devices context out-device compiled-program built-program kernel )))
