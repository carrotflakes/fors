(defpackage fors-test
  (:use :cl
        :prove)
  (:import-from :fors
                :with-fors
                :dump
                :*stack*
                :+open-paren+
                :+close-paren+))
(in-package :fors-test)

;; NOTE: To run this test file, execute `(asdf:test-system :fors)' in your Lisp.

(plan nil)

(setf [ +open-paren+
      ] +close-paren+)

(is (with-fors ()
      (fors:run '(1 2))
      (dump *stack*))
    '(1 2))

(is (with-fors ()
      (fors:run '(1 2 +))
      (dump *stack*))
    '(3))

(is (with-fors ()
      (fors:run `(,[ ,] ))
      (dump *stack*))
    '(2))

(is (with-fors ()
      (fors:run `(,[ add1 1 + ,] ))
      (dump *stack*))
    '(2))

(is (with-fors ()
      (fors:run `(,[ add1 1 + ,] fors.builtin-words::def 2 add1))
      (dump *stack*))
    '(3))

(finalize)
