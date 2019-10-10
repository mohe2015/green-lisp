(defpackage green-lisp/tests/bits
  (:use :cl21
        :green-lisp.bits
        :rove))
(in-package :green-lisp/tests/bits)

;; NOTE: To run this test file, execute `(asdf:test-system :green-lisp)' in your Lisp.

(deftest file->byte-pairs
  (let ((file (merge-pathnames "tests/test.bin" (asdf:system-source-directory :green-lisp/tests))))
    (ok (equal (file->byte-pairs file) '((12 148) (75 0) (12 148) (75 0) (12 148) (75 0) (12 148) (75 0) (12 148)
					 (75 0) (12 148) (75 0) (12 148) (75 0) (12 148) (75 0) (12 148) (75 0)
					 (12 148) (75 0) (12 148) (75 0) (12 148) (75 0) (12 148) (75 0) (12 148)
					 (75 0) (12 148) (75 0) (12 148) (75 0) (12 148) (75 0) (12 148) (75 0)
					 (12 148) (70 0) (12 148) (75 0) (12 148) (75 0) (12 148) (75 0) (12 148)
					 (75 0) (12 148) (75 0) (12 148) (75 0) (12 148) (75 0) (12 148) (75 0)
					 (12 148) (75 0) (12 148) (75 0) (12 148) (75 0) (12 148) (75 0) (12 148)
					 (75 0) (12 148) (75 0) (12 148) (75 0) (12 148) (75 0) (143 147) (140 177)
					 (140 185) (143 145) (24 149) (120 148) (137 225) (137 189) (136 233) (138 189)
					 (12 148) (80 0))))))

(deftest file->bytes
  (let ((file (merge-pathnames "tests/test.bin" (asdf:system-source-directory :green-lisp/tests))))
    (ok (equal (file->bytes file) '(148 12 0 75 148 12 0 75 148 12 0 75 148 12 0 75 148 12 0 75 148 12 0 75 148 12
				    0 75 148 12 0 75 148 12 0 75 148 12 0 75 148 12 0 75 148 12 0 75 148 12 0 75
				    148 12 0 75 148 12 0 75 148 12 0 75 148 12 0 75 148 12 0 75 148 12 0 70 148 12
				    0 75 148 12 0 75 148 12 0 75 148 12 0 75 148 12 0 75 148 12 0 75 148 12 0 75
				    148 12 0 75 148 12 0 75 148 12 0 75 148 12 0 75 148 12 0 75 148 12 0 75 148 12
				    0 75 148 12 0 75 148 12 0 75 147 143 177 140 185 140 145 143 149 24 148 120
				    225 137 189 137 233 136 189 138 148 12 0 80)))))
(deftest byte->bits
  (ok (equal (byte->bits 255) '(1 1 1 1 1 1 1 1))))