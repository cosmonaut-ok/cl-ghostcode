Very tiny library for adding functions, macros and variables as data objects (ghost objects) with possibility to expand it to real
functions, macros or variables. Ex. ghostfunction can be expanded to ordinary function, lambda function, macro, or/and
flet/labels/macrolet region at many different places or your program.

ghostcode could make your code much more dynamic than before!


List of functions and macros:

Methods for working with ghost objects
* g-name
* g-args
* g-body
* g-package
* g-documentation

* define-ghost
* defvar-ghost
* defun-ghost
* ghost-as-var
* ghost-as-parameter
* ghost-as-constant
* ghosts-as-vars
* ghosts-as-parameters
* ghosts-as-constants
* ghost-as-let
* ghost-as-let*
* ghost-as-macrolet
* ghosts-as-let
* ghosts-as-let*
* ghosts-as-macrolet
* ghost-as-function
* ghost-as-macro
* ghost-as-lambda
* ghosts-as-functions
* ghosts-as-macros
* ghosts-as-flet
* ghosts-as-labels
* ghosts-as-macrolet

Example:

```lisp
(use-package :cl-ghostcode)

(defvar *funcs-hash* (make-hash-table :test 'equal))

(setf (gethash "func-1" *funcs-hash*) ;; define ghost object, named ``func-1'' and put it to hashtable
    (define-ghost 'func-1
        :args '(a)
        :body '((format t "~a!!!~%" a) a)) ;; function prints argument to standard output then gives this argument
CL-USER> (find-symbol "FUNC-1") -> NIL ;; Function with name ``func-1'' does not exists
CL-USER> (gethash "func-1" *funcs-hash*) -> #<CL-GHOSTCODE::GHOSTFUNC {10071E7F83}> ;; ghost object exists
CL-USER> (ignore-errors (func-1) func1) ;; check if function, or variable exists
; in: IGNORE-ERRORS (FUNC-2)
;     (CL-GHOSTCODE::FUNC-2)
; 
; caught STYLE-WARNING:
;   undefined function: FUNC-2

;     (PROGN (CL-GHOSTCODE::FUNC-2) CL-GHOSTCODE::FUNC2)
; 
; caught WARNING:
;   undefined variable: FUNC2
; 
; compilation unit finished
;   Undefined function:
;     FUNC-2
;   Undefined variable:
;     FUNC2
;   caught 1 WARNING condition
;   caught 1 STYLE-WARNING condition
NIL
#<UNDEFINED-FUNCTION FUNC-2 {10048FF133}>
CL-USER> (ghost-as-function (gethash "func-1" *funcs-hash*)) ;; expand ghost object to ordinary function
CL-USER> (func-1 10) ;; function with name ``func-1'' now exists
10!!!
10
CL-USER> (func-1 20)
20!!!
20
CL-USER> (ghost-as-flet (gethash "func-1" *funcs-hash*))
				(format nil "Exit from func-1: ~a" (func-1 10)) ;; expand ghost object to ``let'' binding
10!!!
"Exit from func-1: 10"
CL-USER> (macroexpand '(ghost-as-flet (gethash "func-1" *funcs-hash*)) ;; let`s try to expand this macro
					      (format nil "Exit from func-1: ~a" (func-1 10)))
(FLET ((FUNC-1 (A)
	       (FORMAT T "~a!!!~%" A) A))
      (FORMAT NIL "Exit from func-1: ~a" (FUNC-1 10)))
T

CL-USER> (mapcar (ghost-as-lambda (gethash "FUNC-1" *funcs-hash*))
                   '(1 2 3 4 5))
1!!!
2!!!
3!!!
4!!!
5!!!
(1 2 3 4 5)
```

