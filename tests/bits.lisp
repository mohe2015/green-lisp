(defpackage green-lisp/tests/bits
  (:use :cl21
        :green-lisp.bits
        :rove))
(in-package :green-lisp/tests/bits)

;; NOTE: To run this test file, execute `(asdf:test-system :green-lisp)' in your Lisp.

(deftest file->byte-pairs
  (let ((file (merge-pathnames "tests/test.bin" (asdf:system-source-directory :green-lisp/tests))))
    (ok (equal (file->byte-pairs file) '((12 148) (75 0))))))

(deftest file->bytes
  (let ((file (merge-pathnames "tests/test.bin" (asdf:system-source-directory :green-lisp/tests))))
    (ok (equal (file->bytes file) '(148 12 0 75)))))

(deftest byte->bits
  (ok (equal (byte->bits 255) '(1 1 1 1 1 1 1 1)))
  (ok (equal (byte->bits 0) '(0 0 0 0 0 0 0 0)))
  (ok (equal (byte->bits 31) '(0 0 0 1 1 1 1 1))))

(deftest bytes->bits
  (ok (equal (bytes->bits '(255 0 31)) '(1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1))))

(deftest file->bits
  (let ((file (merge-pathnames "tests/test.bin" (asdf:system-source-directory :green-lisp/tests))))
    (ok (equal (file->bits file) '(1 0 0 1 0 1 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 1 1)))))
