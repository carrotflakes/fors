(defpackage fors.exec
  (:use :cl
        :fors.data)
  (:export #:nump 
           #:exec))
(in-package :fors.exec)

(defun nump (x)
  (< x *word-offset*))

(defun exec (start end)
  ;(declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (loop
    for *pointer* from start below end
    for x = (aref *text* *pointer*)
;do(print (list x *pointer* *stack*))
    if (nump x)
    do (vector-push x *stack*)
    else
    do (let* ((index (- x *word-offset*))
              (start (aref *dictionary-start* index)))
         (if (zerop start)
             (funcall (aref *builtin-word-functions* index))
             (exec start (aref *dictionary-end* index))))))
