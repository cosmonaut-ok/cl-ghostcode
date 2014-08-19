Very tiny library for adding functions, macros and variables as data objects (ghost objects) with possibility to expand it to real
functions, macros or variables. Ex. ghostfunction can be expanded to ordinary function, lambda function, macro, or/and
flet/labels/macrolet region at many different places or your program.

ghostcode could make your code much more dynamic than before!


List of functions and macros:

* defvar-ghost
* expand-ghost-to-var
* expand-ghost-to-parameter
* expand-ghosts-to-vars
* expand-ghosts-to-parameters
* expand-ghosts-to-let
* expand-ghosts-to-let*
* defun-ghost
* expand-ghost-to-func
* expand-ghost-to-macro
* expand-ghost-to-lambda
* expand-ghosts-to-funcs
* expand-ghosts-to-macros
* expand-ghosts-to-flet
* expand-ghosts-to-labels
* expand-ghosts-to-macros


Example:

```lisp
(use-package :cl-ghostcode)

(defvar *funcs-hash* (make-hash-table :test 'equal))

(setf (gethash "func-1" *funcs-hash*) ;; define ghost object, named ``func-1'' and put it to hashtable
  (defun-ghost func-1 (a)
    (format t "~a!!!~%" a) a)) ;; function prints argument to standard output then gives this argument

CL-USER> (find-symbol "FUNC-1") -> NIL ;; Function with name ``func-1'' does not exists
CL-USER> (gethash "func-1" *funcs-hash*) -> #<CL-GHOSTCODE::GHOSTFUNC {10071E7F83}> ;; ghost object exists
CL-USER> (expand-ghost-to-func (gethash "func-1" *funcs-hash*)) ;; expand ghost object to ordinary function
CL-USER> (find-symbol "FUNC-1") ;; function with name ``func-1'' now exists
FUNC-1
:INTERNAL
CL-USER> (func-1 20)
20!!!
NIL
CL-USER> (expand-ghosts-to-flet (list (gethash "func-1" *funcs-hash*))
				(format nil "Exit from func-1: ~a" (func-1 10))) ;; expand ghost object to ``let'' binding
10!!!
"Exit from func-1: 10"
CL-USER> (macroexpand '(expand-ghosts-to-flet (list (gethash "func-1" *funcs-hash*)) ;; let`s try to expand this macro
					      (format nil "Exit from func-1: ~a" (func-1 10))))
(FLET ((FUNC-1 (A)
	       (FORMAT T "~a!!!~%" A)))
      (FORMAT NIL "Exit from func-1: ~a" (FUNC-1 10)))
T

CL-USER> (eval 
          `(mapcar ,(expand-ghost-to-lambda (gethash "func-1" *funcs-hash*))
                   '(1 2 3 4 5))) ;; expand ghost object, then eval ``mapcar''
1!!!
2!!!
3!!!
4!!!
5!!!
(1 2 3 4 5)

```
