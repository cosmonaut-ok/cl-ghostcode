;; Copyright (C) 2014 Alexander aka CosmonauT Vynnyk
;;
;;  This file is part of common lisp ghostcode library.
;;
;; ghostcode is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; ghostcode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

(defpackage :cl-ghostcode
  (:nicknames :ghostcode :g-c)
  (:use :cl)
  (:export :ghost
	   :g-name
	   :g-args
	   :g-body
	   :g-package
	   :g-documentation
	   :define-ghost
	   :defvar-ghost
	   :defun-ghost
	   :clone-ghost
		 :ghostlist
	   :ghost-as-var
	   :ghost-as-parameter
	   :ghost-as-constant
	   :ghosts-as-vars
	   :ghosts-as-parameters
	   :ghosts-as-constants
	   :ghost-as-let
	   :ghost-as-let*
	   :ghost-as-macrolet
	   :ghosts-as-let
	   :ghosts-as-let*
	   :ghosts-as-macrolet ;; ??? does it exists
	   :ghost-as-function
	   :ghost-as-macro
	   :ghost-as-lambda
	   :ghosts-as-functions
	   :ghosts-as-macros
	   ;; ghosts-as-lambdas shouldn't be useful
	   :ghosts-as-flet
	   :ghosts-as-labels
	   :ghosts-as-macrolet))

(in-package :cl-ghostcode)

;;;; classes/types/definitions etc
(defclass ghost ()
  ((name :initarg :name :accessor g-name)
   (args :initarg :args :accessor g-args)
   (body :initarg :body :accessor g-body)
   (package :initarg :package :accessor g-package)
   (documentation :initarg :documentation :accessor g-documentation)))

(defun ghost-list-p (list)
  "Need as test function for data type ``ghost''"
  (cond ((null list) t)
				((equal (symbol-name (class-name (class-of (car list)))) "GHOST")
				 (ghost-list-p (cdr list)))
				(t nil)))

(deftype ghostlist ()
  '(and list (satisfies ghost-list-p)))

(defun define-ghost (name &key args body doc)
  "Low-level function for ghost objects definition.
Useful it for automatic ghost objects generation"
  (check-type name symbol)
  (check-type args (or null list))
  (check-type doc (or null string))
  (make-instance 'ghost
		 :name (symbol-name name)
		 :args args :body body :documentation doc
		 :package (symbol-package name)))

(defmacro defvar-ghost (name &optional val doc)
  "Just macro for better usability. In fact, it doesn`t create
some special objects, except standard ghost objects"
  `(define-ghost ,name :body ,val :doc ,doc))

(defmacro defun-ghost (name args &body body)
  "Just macro for better usability. In fact, it doesn`t create
some special objects, except standard ghost objects"
  `(define-ghost ,name :args ,args :body ,body))

(defmethod clone-ghost ((ghost ghost))
  "Clone ghost object"
  (define-ghost (g-name ghost)
      :args (g-args ghost)
      :body (g-body ghost)
      :doc (g-documentation ghost)))

;;;; variables/constants part
(defmethod ghost-as-var ((obj ghost))
  "Expand ghost object to regular global variable"
  (let ((*package* (g-package obj)))
    (eval `(defvar ,(intern (g-name obj))
	     ,(g-body obj) ,(g-documentation obj)))))

(defmethod ghost-as-parameter ((obj ghost))
  "Expand ghost object to regular global variable
 using ``defparameter'' instead of ``defvar''"
  (let ((*package* (g-package obj)))
    (eval `(defparameter ,(intern (g-name obj))
	     ,(g-body obj) ,(g-documentation obj)))))

(defmethod ghost-as-constant ((obj ghost))
  "Expand ghost object to regular global constant"
  (let ((*package* (g-package obj)))
    (eval `(defconstant ,(intern (g-name obj))
	     ,(g-body obj) ,(g-documentation obj)))))

(defmacro ghosts-as-vars (ghosts)
  "Macro for mass ghost objects expansion to variables"
  `(check-type ,ghosts ghostlist)
  `(dolist (var ',(eval (cons 'list ghosts)))
     (let ((*package* (g-package var)))
       (eval `(defvar ,(intern (g-name var))
		,(g-body var) ,(g-documentation var))))))

(defmacro ghosts-as-parameters (ghosts)
  "Macro for mass ghost objects expansion to variables using ``defparameter''"
  `(check-type ,ghosts ghostlist)
  `(dolist (var ',(eval (cons 'list ghosts)))
     (let ((*package* (g-package var)))
       (eval `(defparameter ,(intern (g-name var))
		,(g-body var) ,(g-documentation var))))))

(defmacro ghosts-as-constants (ghosts)
  "Macro for mass ghost objects expansion to constants"
  `(check-type ,ghosts ghostlist)
  `(dolist (var ',(eval (cons 'list ghosts)))
     (let ((*package* (g-package var)))
       (eval `(defconstant ,(intern (g-name var))
		,(g-body var) ,(g-documentation var))))))

(defmethod make-let ((obj ghost))
  "Make component for let/let* environment from ghost object"
  (eval `(let ((*package* ,(g-package obj)))
	   '(,(intern (g-name obj)) ,(g-body obj)))))

(defmethod ghost-as-let ((obj ghost) body)
  "Expand ghost object to ``let'' environment and put body there"
  (eval `(let (,(make-let obj)) ,body)))

(defmethod ghost-as-let* ((obj ghost) body)
  "Expand ghost object to ``let*'' environment and put body there"
  (eval `(let* (,(make-let obj)) ,body)))

(defmethod ghost-as-macrolet ((obj ghost) body)
  "Expand ghost object to ``macrolet'' environment and put body there"
  (eval `(macrolet (,(make-let obj)) ,body)))

(defmacro ghosts-as-let (ghosts &body body)
  "Macro for multiple ghost objects expansion to single ``let'' environment"
  ;; usage: (ghosts-as-let (ghost1 ghost2 etc) (here) (is a body))
  `(check-type ,ghosts ghostlist)
  `(let ,(mapcar #'make-let (eval (cons 'list ghosts)))
     ,@body))

(defmacro ghosts-as-let* (ghosts &body body)
  "Macro for multiple ghost objects expansion to single ``let*'' environment"
  ;; usage: (ghosts-as-let (ghost1 ghost2 etc) (here) (is a body))
  (check-type ghosts ghostlist)
  `(let* ,(mapcar #'make-let (eval (cons 'list ghosts)))
     ,@body))

;;;; functions/macros part

(defmethod ghost-as-function ((func ghost))
  "Expand ghost to ordinary global function"
  (let ((*package* (g-package func)))
    (eval `(defun ,(intern (g-name func)) ,(g-args func) ,@(g-body func)))))

(defmethod ghost-as-macro ((func ghost))
  "Expand ghost to ordinary global macro"
  (let ((*package* (g-package func)))
    (eval `(defmacro ,(intern (g-name func)) ,(g-args func) ,@(g-body func)))))

(defmethod ghost-as-lambda ((func ghost))
  "Expand ghost to lambda function"
  (let ((*package* (g-package func)))
    (eval `(lambda ,(g-args func) ,@(g-body func)))))

(defmacro ghosts-as-functions (ghosts)
  "Macro for mass ghost objects expansion to global functions"
  `(check-type ,ghosts ghostlist)
  `(dolist (func ',(eval (cons 'list ghosts)))
     (let ((*package* (g-package func)))
       (eval `(defun ,(intern (g-name func)) ,(g-args func) ,@(g-body func))))))

(defmacro ghosts-as-macros (ghosts)
  "Macro for mass ghost objects expansion to global macros"
  `(check-type ,ghosts ghostlist)
  `(dolist (func ',(eval (cons 'list ghosts)))
     (let ((*package* (g-package func)))
       (eval `(defmacro ,(intern (g-name func)) ,(g-args func) ,@(g-body func))))))

(defmethod make-flet ((ghost ghost))
  (eval `(let ((*package* ,(g-package ghost)))
	   '(,(intern (g-name ghost)) ,(g-args ghost) ,@(g-body ghost)))))

(defmacro ghosts-as-flet (ghosts &body body)
  "Macro for multiple ghost objects expansion to single ``flet'' environment"
  `(check-type ,ghosts ghostlist)
  `(flet ,(mapcar #'make-flet (eval (cons 'list ghosts)))
     ,@body))

(defmacro ghosts-as-labels (ghosts &body body)
  "Macro for multiple ghost objects expansion to single ``labels'' environment"
  `(check-type ,ghosts ghostlist)
  `(labels ,(mapcar #'make-flet (eval (cons 'list ghosts)))
     ,@body))

(defmacro ghosts-as-macrolet (ghosts &body body)
  "Macro for multiple ghost objects expansion to single ``macrolet'' environment"
  `(check-type ,ghosts ghostlist)
  `(macrolet ,(mapcar #'make-flet (eval (cons 'list ghosts)))
     ,@body))
