;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage schema-system
  (:use :common-lisp
	:asdf))

(in-package :schema-system)

(defsystem "schema"
    :name "schema"
    :version "0.0.0"
    :maintainer "Alex Benishek <abenishe@u.rochester.edu>"
    :author "Alex Benishek <abenishe@u.rochester.edu>"
    :licence "GPLv3"
    :description "Schema Reasoning"
    :long-description "Implementing schema based logical reasoning"
    :depends-on ("trivia")
    :components ((:file "fol")
		 (:file "unifier" :depends-on ("fol"))
		 (:file "main" :depends-on ("unifier" "fol")))
    :entry-point "main:main")
