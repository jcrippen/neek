;;;; -*- Mode: Common-Lisp -*-
;;;;
;;;; Neek: Tlingit corpus utility.
;;;;
;;;; Searching for things in the corpus.

(defgeneric search-corpus (corpus string)
  (:documentation
"Searches all files in the corpus CORPUS for a string STRING. Returns a list
of lines that contain STRING."))

(defgeneric search-corpus-text (corpus string)
  (:documentation
"Searches all text files in the corpus CORPUS for a string STRING. Returns a
list of lines that contain STRING."))

(defgeneric search-corpus-trans (corpus string)
  (:documentation
"Searches all translation files in the corpus CORPUS for a string STRING.
Returns a list of lines that contain STRING."))

(defgeneric re-search-corpus (corpus regexp)
  (:documentation
"Searches all files in the corpus CORPUS for a regular expression REGEXP.
Returns a list of lines that match REGEXP."))

(defgeneric re-search-corpus-text (corpus regexp)
  (:documentation
"Searches all text files in the corpus CORPUS for a regular expression REGEXP.
Returns a list of lines that match REGEXP."))

(defgeneric re-search-corpus-trans (corpus regexp)
  (:documentation
"Searches all translation files in the corpus CORPUS for a regular expression
REGEXP. Returns a list of lines that match REGEXP."))

(defgeneric present-search-result (result stream)
  (:documentation
"Formats a search result (a list of lines) and outputs it to the stream STREAM.
The kind of stream determines the format of the output."))
