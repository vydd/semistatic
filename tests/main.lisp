(defpackage semistatic/tests/main
  (:use :cl
        :semistatic
        :rove))
(in-package :semistatic/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :semistatic)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
