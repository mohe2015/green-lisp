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

(deftest file->bit-reader
  (let* ((file (merge-pathnames "tests/test.bin" (asdf:system-source-directory :green-lisp/tests)))
	 (bit-reader (file->bit-reader file)))
    (ok (read-bit bit-reader) 1)
    (ok (read-bit bit-reader) 0)
    (ok (read-bit bit-reader) 0)
    (ok (read-bit bit-reader) 1)
    (ok (read-bit bit-reader) 0)
    (ok (read-bit bit-reader) 1)
    (ok (read-bit bit-reader) 0)
    (ok (read-bit bit-reader) 0)))

(deftest bits->bits-list
  (ok (equal (bits->bits-list '(1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)) '((0 0 0 0 0 0 0 0) (1 1 1 1 1 1 1 1)))))

(deftest bits->byte
  (ok (equal (bits->byte '(1 1 1 1 1 1 1 1)) 255))
  (ok (equal (bits->byte '(0 0 0 0 0 0 0 0)) 0))
  (ok (equal (bits->byte '(0 0 0 1 1 1 1 1)) 31)))

(deftest bits-list->bytes
  (ok (equal (bits-list->bytes '((0 0 0 0 0 0 0 0) (1 1 1 1 1 1 1 1))) '(0 255))))

(deftest bit-writer->bytes
  (let* ((bit-writer (make-instance 'bit-writer)))
    (loop repeat 8 do
      (write-bit bit-writer 0))
    (loop repeat 8 do
      (write-bit bit-writer 1))
    (ok (equal (bit-writer->bytes bit-writer) '(255 0)))))

;; TODO bytes->file
;; TODO bit-writer->file
