(defpackage fors.exec
  (:use :cl
        :fors.data)
  (:export #:nump 
           #:exec))
(in-package :fors.exec)

(declaim (inline nump))
(defun nump (x)
  (< x *word-offset*))

(declaim (inline proc-word))
(defun proc-word (word)
  (declare (optimize (speed 3) (space 0) (safety 2) (debug 0))
           (type (vector fixnum *) *dictionary-start* *dictionary-end*))
  (let* ((index (- word *word-offset*))
         (start (aref *dictionary-start* index)))
    (if (zerop start)
        (funcall (aref *builtin-word-functions* index))
        (exec start (aref *dictionary-end* index)))))

(defun exec (start end)
  (declare (optimize (speed 3) (space 0) (safety 2) (debug 0))
           (type fixnum start end)
           (type (vector (signed-byte 32) *) *text* *stack*))
  (loop
    for *pointer* fixnum from start below end
    for x = (aref *text* *pointer*)
;do(print (list x *pointer* *stack*))
    if (nump x)
    do (vector-push x *stack*)
    else
    do (proc-word x)))
