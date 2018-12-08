;;;; -*- Mode: Common-Lisp -*-
;;;;
;;;; Neek: Tlingit corpus utility.
;;;;
;;;; Configuration variables.
;;;;

(in-package "NEEK")

(defvar *corpus-dir* "/Users/james/Documents/Mine/tlingit-corpus/")

(defvar +whitespace+ (cl-unicode:list-all-characters "White_Space")
"A list of whitespace characters. These are obtained from CL-UNICODE by
listing all Unicode characters with the White_Space property.")
