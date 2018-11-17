(defpackage #:unifier
  (:documentation "An implmentation of schema based reasoning.")
  (:use :common-lisp
        :trivia
	:fol)
  (:export most-general-unifier))

(in-package :unifier)
(import 'fol:fol-variable)
(import 'fol:fol-function)
(import 'fol:fol-predicate)
(import 'fol:name)
(import 'fol:args)
(import 'fol:fol-eq)
(declaim (optimize (speed 0) (space 0) (safety 3) (debug 3)))

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

(defun most-general-unifier (a b)
  (let (
        (unifier (make-unifier))
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
      (with-slots ((a-name name) (a-args args)) a
	(with-slots ((b-name name) (b-args args)) b
	  (if (equal a-name b-name)
              (make-instance 'fol-predicate :name a-name :args (mapcar #'unify-values a-args b-args))
              (error "Cannot unify different predicates")))))))
