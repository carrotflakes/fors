(defpackage fors.builtin-words
  (:use :cl
        :fors.data
        :fors.exec)
  (:export #:*builtin-words*
           #:*builtin-word-functions*))
(in-package :fors.builtin-words)

(defmacro defword (symbol &body body)
  `(progn
     (setf *builtin-words* (nconc *builtin-words* (list ',symbol)))
     (vector-push
      (lambda ()
        ,@body
        nil)
      *builtin-word-functions*)))


(eval `(defword ,+open-paren+
         (vector-push (+ *pointer* 2) *stack*)
         (setf *pointer* (aref *text* (1+ *pointer*)))))

(eval `(defword ,+close-paren+
         (setf *pointer* *text-size*)))


(defword swap
  (let* ((l (length *stack*))
         (tmp (aref *stack* (- l 1))))
    (setf (aref *stack* (- l 1)) (aref *stack* (- l 2))
          (aref *stack* (- l 2)) tmp)))

(defword rot
  (let* ((l (length *stack*))
         (tmp (aref *stack* (- l 1))))
    (setf (aref *stack* (- l 1)) (aref *stack* (- l 3))
          (aref *stack* (- l 3)) tmp)))

(defword dup
  (vector-push (aref *stack* (1- (length *stack*))) *stack*))

(defword drop
  (vector-pop *stack*))


(defword def
  (let* ((pointer (vector-pop *stack*))
         (word (aref *text* pointer))
         (index (- word *word-offset*)))
    (print (list pointer word index))
    (setf (aref *dictionary-start* index) (1+ pointer)
          (aref *dictionary-end* index) (1- *pointer*))))

(defword run
  'TODO)

(defword if
  (let ((pointer (vector-pop *stack*))
        (cond (vector-pop *stack*)))
    (when (/= cond 0)
      (exec pointer (1- *pointer*)))))

(defword if-else
  (let ((pointer-else (vector-pop *stack*))
        (pointer-then (vector-pop *stack*))
        (cond (vector-pop *stack*)))
    (if (= cond 0)
      (exec pointer-else (1- *pointer*))
      (exec pointer-then (1- *pointer*)))))


(defword p
  (print (vector-pop *stack*)))

(defword ps
  (print *stack*))


(defword +
  (vector-push (+ (vector-pop *stack*) (vector-pop *stack*))
               *stack*))

(defword -
  (vector-push (- (vector-pop *stack*) (vector-pop *stack*))
               *stack*))

(defword =
  (vector-push (if (= (vector-pop *stack*) (vector-pop *stack*)) 1 0)
               *stack*))

(defword !=
  (vector-push (if (/= (vector-pop *stack*) (vector-pop *stack*)) 1 0)
               *stack*))

(defword <
  (vector-push (if (< (vector-pop *stack*) (vector-pop *stack*)) 1 0)
               *stack*))

(defword <=
  (vector-push (if (<= (vector-pop *stack*) (vector-pop *stack*)) 1 0)
               *stack*))