;;; -*- Mode: Lisp -*-

(defpackage :ghostcode-system
  (:use :cl :asdf))

(in-package :ghostcode-system)

(defsystem :ghostcode
  :name "GHOSTCODE"
  :author "Alexander aka 'CosmonauT' Vynnyk <cosmonaut.ok@zoho.com>"
  :version "0.1"
  :maintainer "Alexander aka 'CosmonauT' Vynnyk <cosmonaut.ok@zoho.com>"
  :license "GNU General Public License"
  :description "A tiny library for definition functions as objects for future expansion to ordinary functions"
  :serial t
  :components ((:file "ghostcode")))

