(defpackage green-lisp.serial-interface
  (:use :cl :cffi))
(in-package :green-lisp.serial-interface)

(define-foreign-library libserial-interface
  (t (:default "libserial-interface")))
(use-foreign-library libserial-interface)
