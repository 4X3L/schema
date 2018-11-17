(defpackage #:main
  (:documentation "An implmentation of shcema based reasoning.")
  (:use :common-lisp :schema)
  (:export main))

(in-package :main)

(defun main ()
  (let* (
	 (a (make-instance 'schema:fol-variable :name "a"))
	 (b (make-instance 'schema:fol-variable :name "b"))
	 (c (make-instance 'schema:fol-function :name "c"))
	 (d (make-instance 'schema:fol-function :name "d"))
	 (y (make-instance 'schema:fol-predicate :name "P" :args (list a c)))
	 (x (make-instance 'schema:fol-predicate :name "P" :args (list d b)))
	 (z (schema:most-general-unifier y x)))
    (print-object z *standard-output*)))
