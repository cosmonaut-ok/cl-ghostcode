;; Copyright (C) 2014 Alexander aka CosmonauT Vynnyk
;;
;;  This file is common lisp ghostcode library.
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
  (:use :cl))

(in-package :cl-ghostcode)

(export '(defvar-ghost
	  expand-ghost-to-var
	  expand-ghost-to-parameter
	  expand-ghosts-to-vars
	  expand-ghosts-to-parameters
	  expand-ghosts-to-let
	  expand-ghosts-to-let*
	  defun-ghost
	  expand-ghost-to-func
	  expand-ghost-to-macro
	  expand-ghost-to-lambda
	  expand-ghosts-to-funcs
	  expand-ghosts-to-macros
	  expand-ghosts-to-flet
	  expand-ghosts-to-labels
	  expand-ghosts-to-macrolet))

;;;; classes
(defclass ghostvar ()
  ((name :initarg :name :accessor g-name)
   (value :initarg :value :accessor g-value)
   (package :initarg :package :accessor g-package)
   (documentation :initarg :documentation :accessor g-documentation)))

(defclass ghostfunc ()
  ((name :initarg :name :accessor g-name)
   (package :initarg :package :accessor g-package)
   (args :initarg :args :accessor g-args)
   (body :initarg :body :accessor g-body)))

;;;; ghostvar

(defmacro defvar-ghost (var &optional val doc)
  "Makes ghostvar - ghost variable"
  `(check-type ,var symbol)
  `(check-type ,doc (or null string))
  `(make-instance 'ghostvar
		  :name (symbol-name ',var)
		  :value ,val
		  :package (symbol-package ',var)
		  :documentation ,doc))

(defmethod expand-ghost-to-var ((var ghostvar))
  (let ((*package* (g-package var)))
    (eval `(defvar ,(intern (g-name var)) ,(g-value var) ,(g-documentation var)))))

(defmethod expand-ghost-to-parameter ((var ghostvar))
  (let ((*package* (g-package var)))
    (eval `(defparameter ,(intern (g-name var)) ,(g-value var) ,(g-documentation var)))))

(defmacro expand-ghosts-to-vars (ghostvars)
  `(check-type ghostvars list)
  `(dolist (var ,ghostvars)
     (let ((*package* (g-package var)))
       (eval `(defvar ,(intern (g-name var)) ,(g-value var) ,(g-documentation var))))))

(defmacro expand-ghosts-to-parameters (ghostvars)
  `(check-type ghostvars list)
  `(dolist (var ,ghostvars)
     (let ((*package* (g-package var)))
       (eval `(defparameter ,(intern (g-name var)) ,(g-value var) ,(g-documentation var))))))

(defmethod make-let ((ghostvar ghostvar))
  (eval `(let ((*package* ,(g-package ghostvar)))
	   '(,(intern (g-name ghostvar)) ,(g-value ghostvar)))))

(defmacro expand-ghosts-to-let (ghostvars &body body)
  `(let ,(mapcar #'make-let (eval ghostvars))
     ,@body))

(defmacro expand-ghosts-to-let* (ghostvars &body body)
  `(let* ,(mapcar #'make-let (eval ghostvars))
     ,@body))



;; expand-ghosts-to-let*
(in-package :cl-ghostcode)

(defmacro defun-ghost (name args &body body) ;; name args [package] body
  "Makes ghostfun - ghost function"
  `(check-type name symbol)
  `(check-type args (or null list))
  `(make-instance 'ghostfunc
		  :name ,(symbol-name name)
		  :args ',args
		  :body ',body
		  :package ,(symbol-package name)))

(defmethod expand-ghost-to-func ((func ghostfunc))
  "Expand ghost to ordinary function"
  (let ((*package* (g-package func)))
    (eval `(defun ,(intern (g-name func)) ,(g-args func) ,@(g-body func)))))

(defmethod expand-ghost-to-macro ((func ghostfunc))
  "Expand ghost to ordinary macro"
  (let ((*package* (g-package func)))
    `(defmacro ,(intern (g-name func)) ,(g-args func) ,@(g-body func))))

(defmethod expand-ghost-to-lambda ((func ghostfunc))
  "Expand ghost to lambda function"
  (let ((*package* (g-package func)))
    ;; (eval `(defun ,(intern (g-name func)) ,(g-args func) ,@(g-body func)))))
    (eval `(lambda ,(g-args func) ,@(g-body func)))))

(defmacro expand-ghosts-to-funcs (ghostfuncs)
  `(check-type ,ghostfuncs list)
  `(dolist (func ,ghostfuncs)
     (let ((*package* (g-package func)))
       (eval `(defun ,(intern (g-name func)) ,(g-args func) ,@(g-body func))))))

(defmacro expand-ghosts-to-macros (ghostfuncs)
  `(check-type ,ghostfuncs list)
  `(dolist (func ,ghostfuncs)
     (let ((*package* (g-package func)))
       (eval `(defmacro ,(intern (g-name func)) ,(g-args func) ,@(g-body func))))))

(defmethod make-flet ((ghostfunc ghostfunc))
  (eval `(let ((*package* ,(g-package ghostfunc)))
	   '(,(intern (g-name ghostfunc)) ,(g-args ghostfunc) ,@(g-body ghostfunc)))))

(defmacro expand-ghosts-to-flet (ghostfuncs &body body)
  `(flet ,(mapcar #'make-flet (eval ghostfuncs))
     ,@body))

(defmacro expand-ghosts-to-labels (ghostfuncs &body body)
  `(labels ,(mapcar #'make-flet (eval ghostfuncs))
     ,@body))

(defmacro expand-ghosts-to-macrolet (ghostfuncs &body body)
  `(macrolet ,(mapcar #'make-flet (eval ghostfuncs))
     ,@body))

