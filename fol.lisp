(defpackage #:fol
  (:documentation "Datatypes for first order logic and schemas")
  (:use :common-lisp)
  (:export fol-predicate
	   fol-variable
	   fol-function
	   name
	   args
	   fol-eq))
(in-package :fol)


(defgeneric fol-eq (a b)
  (:documentation "Equality as specified for first order logic formulas."))
(defmethod fol-eq ((a t) (b t))
  nil)

(defclass fol-variable (common-lisp:standard-object)
  ((name
    :initarg :name
    :initform ""
    :type string
    :documentation "The name of the variable as a string")))
(defmethod fol-eq ((a fol-variable) (b fol-variable))
  (equal (slot-value a 'name) (slot-value b 'name)))
(defmethod print-object ((object fol-variable) out)
  (format out "V.~a" (slot-value object 'name)))

(defclass fol-function (standard-object)
  ((name
    :initarg :name
    :initform ""
    :type string
    :documentation "The name of the function as a string")
   (args
    :initarg :args
    :initform '()
    :type list
    :documentation "The arguments to the function as a list of fol-value")))
(defmethod fol-eq ((a fol-function) (b fol-function))
  (and
   (equal (slot-value a 'name) (slot-value b 'name))
   (reduce (lambda (x y) (and x y)) (mapcar #'fol-eq
                                            (slot-value a 'args)
                                            (slot-value b 'args))
           :initial-value t)))
(defmethod print-object ((object fol-function) out)
  (with-slots (name args) object
    (format out "F.~a(~{~a~^,~})" name args)))
(defclass fol-predicate (standard-object)
  ((name
    :initarg :name
    :initform ""
    :type string
    :documentation "The name of the predicate as a string")
   (args
    :initarg :args
    :initform '()
    :type list
    :documentation "The arguments to the predicate as a list of fol-value")))
(defmethod fol-eq ((a fol-predicate) (b fol-predicate))
  (and
   (equal (slot-value a 'name) (slot-value b 'name))
   (reduce (lambda (x y) (and x y)) (mapcar #'fol-eq
                                            (slot-value a 'args)
                                            (slot-value b 'args))
           :initial-value t)))
(defmethod print-object ((object fol-predicate) out)
  (with-slots (name args) object
    (format out "P.~a(~{~a~^,~})" name args)))
