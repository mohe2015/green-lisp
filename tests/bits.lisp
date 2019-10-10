(defpackage green-lisp/tests/bits
  (:use :cl21
        :green-lisp.bits
        :rove))
(in-package :green-lisp/tests/bits)

;; NOTE: To run this test file, execute `(asdf:test-system :green-lisp)' in your Lisp.

(deftest file->byte-pairs
  (let ((file (merge-pathnames "tests/test.bin" (asdf:system-source-directory :green-lisp/tests))))
    (testing "(file->byte-pairs \"test.bin\")"
      (ok (equal (file->byte-pairs file) '((12 148) (75 0) (12 148) (75 0) (12 148) (75 0) (12 148) (75 0) (12 148)
					   (75 0) (12 148) (75 0) (12 148) (75 0) (12 148) (75 0) (12 148) (75 0)
					   (12 148) (75 0) (12 148) (75 0) (12 148) (75 0) (12 148) (75 0) (12 148)
					   (75 0) (12 148) (75 0) (12 148) (75 0) (12 148) (75 0) (12 148) (75 0)
					   (12 148) (70 0) (12 148) (75 0) (12 148) (75 0) (12 148) (75 0) (12 148)
					   (75 0) (12 148) (75 0) (12 148) (75 0) (12 148) (75 0) (12 148) (75 0)
					   (12 148) (75 0) (12 148) (75 0) (12 148) (75 0) (12 148) (75 0) (12 148)
					   (75 0) (12 148) (75 0) (12 148) (75 0) (12 148) (75 0) (143 147) (140 177)
					   (140 185) (143 145) (24 149) (120 148) (137 225) (137 189) (136 233) (138 189)
					   (12 148) (80 0)))))))
