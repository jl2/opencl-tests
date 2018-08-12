;;;; opencl-tests.asd
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(asdf:defsystem #:opencl-tests
  :description "Playing with opencl."
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license  "ISC (BSD-like)"
  :version "0.0.1"
  :serial t
  :depends-on (#:eazy-opencl #:oclcl)
  :components ((:file "package")
               (:file "opencl-tests")))
