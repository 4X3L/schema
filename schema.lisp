(defpackage #:schema
  (:documentation "An implmentation of schema based reasoning.")
  (:use :common-lisp
        :alexandria
        :trivia)
  (:export most-general-unifier
	   fol-predicate
	   fol-variable
	   fol-function))

(in-package :schema)
(declaim (optimize (speed 0) (space 0) (safety 3) (debug 3)))

(defgeneric fol-eq (a b)
  (:documentation "Equality as specified for first order logic formulas."))
(defmethod fol-eq ((a t) (b t))
  nil)

(defclass fol-formula (standard-object)
  ()
  )
(defclass fol-value (fol-formula)
  ()
  )
(defclass fol-statement (fol-formula)
  ()
  )

(defclass fol-variable (fol-value)
  ((name
    :initarg :name
    :initform ""
    :type string
    :documentation "The name of the variable as a string")))
(defmethod fol-eq ((a fol-value) (b fol-value))
  (equal (slot-value a 'name) (slot-value b 'name)))
(defmethod print-object ((object fol-variable) out)
  (format out "V.~a" (slot-value object 'name)))

(defclass fol-function (fol-value)
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
(defclass fol-predicate (fol-statement)
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
(defmethod fol-eq ((a fol-statement) (b fol-statement))
  (and
   (equal (slot-value a 'name) (slot-value b 'name))
   (reduce (lambda (x y) (and x y)) (mapcar #'fol-eq
                                            (slot-value a 'args)
                                            (slot-value b 'args))
           :initial-value t)))
(defmethod print-object ((object fol-predicate) out)
  (with-slots (name args) object
    (format out "P.~a(~{~a~^,~})" name args)))
(defstruct unifier
  "A table for replacing wff by other wff"
  (renames (make-hash-table :test #'equalp) :type hash-table))

(defun add-sym-rename (u k v)
  (let ((rename (find-sym-rename u v)))
    (setf (gethash k (unifier-renames u)) rename)
    rename))
(defun find-sym-rename (u name)
  (let ((rename (gethash name (unifier-renames u))))
    (if rename
        (if (equalp rename name)
            rename
            (find-sym-rename u rename))
        (setf (gethash name (unifier-renames u)) name))))

(defun all-satisfy (pred lst)
  (reduce (lambda (x y) (and x y)) (mapcar pred lst) :initial-value t))

(defun most-general-unifier (a b)
  (let (
        (unifier (make-unifier))
        (a-args (slot-value a 'args))
        (b-args (slot-value b 'args))
        (a-name (slot-value a 'name))
        (b-name (slot-value b 'name))
        )
    (labels
        (
         (set-name (n r)
           (add-sym-rename unifier n r)
           )
         (replace-lit-in (lit wff)
           (if (fol-eq lit wff)
               lit
               (match wff
                      ((class fol-variable)
                       lit)
                      ((class fol-function (name n) (args a))
                       (make-instance 'fol-function
                                      :name n
                                      :args (mapcar (lambda (x) (replace-lit-in lit x))
                                                    a)))
                      ((class fol-predicate (name n) (args a))
                       (make-instance 'fol-predicate
                                      :name n
                                      :args (mapcar (lambda (x) (replace-lit-in lit x))
                                                    a)))
                      (_ (error "Not a variable, function, or predicate."))
                      )))
	 (unify-values (v w)
           (let ((new-name
                  (match v
                    ((class fol-variable)
                     (set-name v w))
                    ((class fol-function (name n) (args x))
                     (match w
                       ((class fol-function (name m) (args y))
                        (if (equalp n m)
                            (make-instance 'fol-function :name n :args (mapcar #'unify-values x y))
                            (error (format nil "Could not unify functions ~a, ~a, as they have different names.~%" v w))))
		       ((class fol-variable)
			(make-instance 'fol-function :name n :args x))
		       (_
			(error (format nil "Could not unify function ~a with ~a, which is neither a variable nor function.~%" v w)))))
                    (_
                     (match w
                       ((class fol-variable)
                        (set-name w v))
                       (_ (error (format nil "Could not unify two unknown objects, ~a, ~a" v w))))))))
             (setf a (replace-lit-in new-name a))
             (setf b (replace-lit-in new-name b))
             new-name)))
      (if (equal a-name b-name)
          (make-instance 'fol-predicate :name a-name :args (mapcar #'unify-values a-args b-args))
          (error "Cannot unify different predicates")))))
