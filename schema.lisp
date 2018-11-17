(defpackage #:schema
  (:documentation "An implmentation of schema based reasoning.")
  (:use :common-lisp
        :alexandria
        :trivia)
  (:export most-general-unifier))

(in-package :schema)
(declaim (optimize (speed 0) (space 0) (safety 3) (debug 3)))

(defgeneric fol-eq (a b)
  (:documentation "Equality as specified for first order logic formulas."))
(defmethod fol-eq ((a standard-object) (b standard-object)))

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

(defstruct unifier
  "A table for replacing wff by other wff"
  (renames (make-hash-table :test #'equalp) :type hash-table))

(defun add-sym-rename (u k v)
  (setf (gethash k (unifier-renames u)) (find-sym-rename u v))
  v)
(defun find-sym-rename (u name)
  (break)
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
         (get-name (n)
           (find-sym-rename unifier n))
         (replace-lit-in (lit wff)
           (if (fol-eq lit wff)
               lit
               (match wff
                      ((class fol-variable (name n))
                       lit)
                      ((class fol-function (name n) (args a))
                       (make-instance 'fol-function
                                      :name n
                                      :args (mapcar (lambda (x) (replace-lit-in lit x))
                                                    x)))
                      ((class fol-predicate (name n) (args a))
                       (make-instance 'fol-predicate
                                      :name n
                                      :args (mapcar (lambda (x) (replace-lit-in lit x))
                                                    x)))
                      (_ (error "Not a variable, function, or predicate."))
                      ))))
      (unify-values (v w)
                    (let ((new-name
                           (match v
                                  ((class fol-variable (name x))
                                   (set-name x w))
                                  ((class fol-function (name n) (args x))
                                   (match w
                                          ((class fol-function (name m) (args y))
                                           (if (equalp n m)
                                               (make-instance 'fol-function :name n :args (mapcar #'unify-values x y))
                                               (error "Could not unify functions with different names")))))
                                  (_
                                   (match w
                                          ((class fol-variable (name y))
                                           (set-name y v))
                                          (_ (error "Could not unify variables")))))))
                      (setf a (replace-lit-in new-name a))
                      (setf b (replace-lit-in new-name b))
                      new-name)))
    (if (equal a-name b-name)
        (make-instance 'fol-pred :name a-name :args (mapcar #'unify-values a-args b-args))
        (error "Cannot unify different predicates"))))
