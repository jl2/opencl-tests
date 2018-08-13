;;;; package.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(defpackage #:opencl-tests
  (:use #:cl #:alexandria)
  (:export #:show-devices
           #:mandelbrot-set))
