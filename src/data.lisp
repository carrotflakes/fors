(defpackage fors.data
  (:use :cl)
  (:export #:+open-paren+
           #:+close-paren+
           #:*word-type*
           #:*word-offset*
           #:*text-size*
           #:*stack-size*
           #:*dictionary-size*
           #:*symbol-table*
           #:*text*
           #:*pointer*
           #:*stack*
           #:*dictionary-start*
           #:*dictionary-end*))
(in-package :fors.data)

(defconstant +open-paren+ '[)
(defconstant +close-paren+ '])

(defvar *word-type* '(signed-byte 64))
(defvar *word-offset* (expt 2 (- 64 2)))
(defvar *text-size* (expt 2 10))
(defvar *stack-size* (expt 2 10))
(defvar *dictionary-size* (expt 2 5))
(defvar *symbol-table* nil)
(defvar *text* nil)
(defvar *pointer* nil)
(defvar *stack* nil)

(defvar *dictionary-start* nil)
(defvar *dictionary-end* nil)