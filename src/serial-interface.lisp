(defpackage green-lisp.serial-interface
  (:use :cl :cffi))
(in-package :green-lisp.serial-interface)

(define-foreign-library libserial-interface
  (t (:default "libserial-interface")))
(use-foreign-library libserial-interface)

(defcfun "serial_connect" :pointer)
(defcfun "serial_read" :int
  (handle :pointer))
(defcfun "serial_write" :bool
  (handle :pointer)
  (char :char))
(defcfun "serial_close" :void
  (handle :pointer))
