;;;; -*- Mode: Common-Lisp -*-
;;;;
;;;; Neek: Tlingit corpus utility.
;;;;
;;;; Corpus definition and top level.

(in-package "NEEK")

(defclass corpus ()
  ((entries :type (vector entry *)
            :documentation "A vector of corpus entries."))
  (:documentation
   "The top-level object that contains a corpus. The ENTRIES slot contains a
vector of ENTRY instances, one for each entry in the corpus."))
