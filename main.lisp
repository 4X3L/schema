(defpackage #:main
  (:documentation "An implmentation of shcema based reasoning.")
  (:use :common-lisp
	:unifier
	:fol)
  (:export main))

(in-package :main)

(defun main ()
  (let* (
	 (a (make-instance 'fol:fol-variable :name "a"))
	 (b (make-instance 'fol:fol-variable :name "b"))
	 (c (make-instance 'fol:fol-function :name "c"))
	 (d (make-instance 'fol:fol-function :name "d"))
	 (y (make-instance 'fol:fol-predicate :name "P" :args (list a c)))
	 (x (make-instance 'fol:fol-predicate :name "P" :args (list d b)))
	 (z (unifier:most-general-unifier y x)))
    (print-object z *standard-output*)))
