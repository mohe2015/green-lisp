(defpackage green-lisp/tests/bits
  (:use :cl21
        :green-lisp
        :rove))
(in-package :green-lisp/tests/bits)

;; NOTE: To run this test file, execute `(asdf:test-system :green-lisp)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
