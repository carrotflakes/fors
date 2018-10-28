#|
  This file is a part of fors project.
  Copyright (c) 2018 carrotflakes (carrotflakes@gmail.com)
|#

(defsystem "fors-test"
  :defsystem-depends-on ("prove-asdf")
  :author "carrotflakes"
  :license "LLGPL"
  :depends-on ("fors"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "fors"))))
  :description "Test system for fors"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
