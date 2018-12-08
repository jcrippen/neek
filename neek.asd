;;;; -*- Mode: Common-Lisp -*-
;;;;
;;;; Neek: Tlingit corpus utility.
;;;;
;;;; ASDF System definition.

(cl:in-package "ASDF-USER")

(defsystem "neek"
  :description "Utility for exploring the Tlingit language corpus."
  :author "James A. Crippen <jcrippen@gmail.com>"
  :licence "CC-BY-SA 4.0"
  :version "0.0.1"
  :depends-on ("uiop"
               "alexandria"
               "cl-unicode"
               "string-case"
               "split-sequence")
  :serial t
  :components ((:file "package")
               (:file "config")
               (:file "lines")
               (:file "files")
               (:file "corpora")))

  ;; :components ((:file "package")
  ;;              (:file "config"  :depends-on ("package"))
  ;;              (:file "lines"   :depends-on ("package"))
  ;;              (:file "files"   :depends-on ("lines"))
  ;;              (:file "corpora" :depends-on ("files"))
  ;;              ))
