(defpackage fors
  (:use :cl
        :fors.data
        :fors.builtin-words
        :fors.exec)
  (:export #:with-fors
           #:dump
           #:run
           #:*stack*
           #:*text*))
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
              ((string= x +open-paren+)
               (vector-push (gethash +open-paren+ *symbol-table*)
                            *text*)
               (push (length *text*) open-paren-pointers)
               (vector-push 0 *text*))
              ((string= x +close-paren+)
               (vector-push (gethash +close-paren+ *symbol-table*)
                            *text*)
               (let ((open-paren-pointer (pop open-paren-pointers)))
                 (setf (aref *text* open-paren-pointer)
                       (1- (length *text*)))))
              ((symbolp x)
               (setf x (intern (symbol-name x) :fors.data))
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

(defun dump (memory)
  (let ((symbols (symbol-table-symbols)))
    (loop
      for x across memory
      collect (if (nump x)
                  x
                  (aref symbols (- x *word-offset*))))))

(defun run (source)
  (let ((start (length *text*)))
    (write-code source)
    (exec start (length *text*))))
