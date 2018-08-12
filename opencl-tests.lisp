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
