(defpackage fors.builtin-words
  (:use :cl
        :fors.data)
  (:export #:*builtin-words*
           #:*builtin-word-functions*))
(in-package :fors.builtin-words)

(defvar *builtin-words* '())
(defvar *builtin-word-functions* (make-array 128 :element-type 'function :fill-pointer 0))

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

(eval `(defword ,+close-paren+ nil))


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
  'TODO)

(defword if-else
  'TODO)


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
