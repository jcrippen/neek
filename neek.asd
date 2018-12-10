;;;; -*- Mode: Common-Lisp -*-
;;;;
;;;; Neek: Tlingit corpus utility.
;;;;
;;;; ASDF System definition.

(cl:in-package "ASDF-USER")

(defsystem "neek"
  :description "Program for exploring the Tlingit language corpus."
  :author "James A. Crippen <jcrippen@gmail.com>"
  :licence "CC-BY-SA 4.0"
  :version "0.0.2"
  :depends-on ("uiop"
               "alexandria"
               "cl-unicode"
               "string-case"
               "split-sequence")
  ;; FIXME: Make not hardcoded serial using :DEPENDS-ON in :COMPONENTS.
  :serial t
  :components ((:file "package")
               (:file "config")
               (:file "lines")
               (:file "files")
               (:file "corpora")))
