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

(is (with-fors ()
      (fors:run `(0 ,[ 1 ,] if))
      (dump *stack*))
    '())

(is (with-fors ()
      (fors:run `(1 ,[ 1 ,] if))
      (dump *stack*))
    '(1))

(is (with-fors ()
      (fors:run `(0 ,[ 1 ,] ,[ 2 ,] fors.builtin-words::if-else))
      (dump *stack*))
    '(2))

(is (with-fors ()
      (fors:run `(1 ,[ 1 ,] ,[ 2 ,] fors.builtin-words::if-else))
      (dump *stack*))
    '(1))

(is (with-fors ()
      (fors:run `(1 fors.builtin-words::dup))
      (dump *stack*))
    '(1 1))

(is (with-fors ()
      (fors:run `(1 2 fors.builtin-words::swap))
      (dump *stack*))
    '(2 1))

(is (with-fors ()
      (fors:run `(1 fors.builtin-words::drop))
      (dump *stack*))
    '())

(is (with-fors ()
      (fors:run `(,[ fib dup 0 swap <= ,[ drop 0 ,] ,[ dup 1 != ,[ dup 1 swap - fib swap 2 swap - fib + ,] if ,] if-else ,] def 0 fib 1 fib 2 fib 3 fib 4 fib 5 fib 6 fib))
      (dump *stack*))
    '(0 1 1 2 3 5 8))

(finalize)
