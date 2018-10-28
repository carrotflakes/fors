#|
  This file is a part of fors project.
  Copyright (c) 2018 carrotflakes (carrotflakes@gmail.com)
|#

#|
  Author: carrotflakes (carrotflakes@gmail.com)
|#

(defsystem "fors"
  :version "0.1.0"
  :author "carrotflakes"
  :license "LLGPL"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "fors" :depends-on ("builtin-words" "exec" "data"))
                 (:file "builtin-words" :depends-on ("exec" "data"))
                 (:file "exec" :depends-on ("data"))
                 (:file "data"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "fors-test"))))
