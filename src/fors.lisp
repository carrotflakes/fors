(defpackage fors
  (:use :cl
        :fors.data
        :fors.builtin-words)
  (:export #:with-fors
           #:dump
           #:run
           #:*stack*
           #:*text*
           #:+open-paren+
           #:+close-paren+))
(in-package :fors)

(defmacro with-fors ((&key (text-size *text-size*)
                           (stack-size *stack-size*)
                           (dictionary-size *dictionary-size*))
                     &body body)
  `(let ((*symbol-table* (make-hash-table :test 'eq))
         (*text* (make-array ,text-size
                             :element-type *word-type*
                             :fill-pointer 0))
         (*stack* (make-array ,stack-size
                              :element-type *word-type* 
                              :fill-pointer 0))
         (*dictionary-start* (make-array ,dictionary-size
                                         :initial-element 0
                                         :element-type 'fixnum
                                         :fill-pointer 0))
         (*dictionary-end* (make-array ,dictionary-size
                                       :initial-element 0
                                       :element-type 'fixnum
                                       :fill-pointer 0)))
     (initialize)
     ,@body))

(defun initialize ()
  (setf (fill-pointer *dictionary-start*) (length *builtin-words*)
        (fill-pointer *dictionary-end*) (length *builtin-words*))
  (loop
    for word in *builtin-words*
    do (setf (gethash word *symbol-table*)
             (+ *word-offset* (hash-table-count *symbol-table*))))
  nil)

(defun write-code (source)
  (let ((open-paren-pointers '()))
    (mapc (lambda (x)
            (cond
              ((typep x *word-type*)
               (vector-push x *text*))
              ((eq x +open-paren+)
               (vector-push (gethash x *symbol-table*)
                            *text*)
               (push (length *text*) open-paren-pointers)
               (vector-push 0 *text*))
              ((eq x +close-paren+)
               (vector-push (gethash x *symbol-table*)
                            *text*)
               (let ((open-paren-pointer (pop open-paren-pointers)))
                 (setf (aref *text* open-paren-pointer)
                       (1- (length *text*)))))
              ((symbolp x)
               (vector-push (if (gethash x *symbol-table*)
                                (gethash x *symbol-table*)
                                (setf (gethash x *symbol-table*)
                                      (+ *word-offset* (hash-table-count *symbol-table*))))
                            *text*))
              (t
               (error "illegal object as word ~a" x))))
          source)))

(defun symbol-table-symbols ()
  (let ((symbols (make-array (hash-table-count *symbol-table*))))
    (maphash (lambda (key value)
               (setf (aref symbols (- value *word-offset*))
                     key))
             *symbol-table*)
    symbols))

(defun nump (x)
  (< x *word-offset*))

(defun dump (memory)
  (let ((symbols (symbol-table-symbols)))
    (loop
      for x across memory
      collect (if (nump x)
                  x
                  (aref symbols (- x *word-offset*))))))

(defun exec (start end)
  ;(declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (loop
    for *pointer* from start below end
    for x = (aref *text* *pointer*)
    if (nump x)
    do (vector-push x *stack*)
    else
    do (let* ((index (- x *word-offset*))
              (start (aref *dictionary-start* index)))
         (if (zerop start)
             (funcall (aref *builtin-word-functions* index))
             (exec start (aref *dictionary-end* index))))))

(defun run (source)
  (let ((start (length *text*)))
    (write-code source)
    (print (dump *text*))
    (exec start (length *text*))
    (print (dump *stack*))))


'
(with-fors ()
  (run '($ fib
           dup 0 swap <= if
             drop 0
           else
             dup 1 = if
               drop 1
             else
               dup 1 swap - fib swap 2 swap - fib +
             then
           then
         $)))
