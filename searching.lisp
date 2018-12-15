;;;; -*- Mode: Common-Lisp -*-
;;;;
;;;; Neek: Tlingit corpus utility.
;;;;
;;;; Searching for things in the corpus.

(in-package "NEEK")

(defclass csearch ()
  ((string :type string
           :accessor csearch-string
           :initarg :string
           :documentation "String for searching.")
   (regexp-p :type (member nil t)
             :accessor csearch-regexp-p
             :initarg :regexp-p
             :documentation "T if search string is a regular expression.")
   (results :type (vector * line)
            :accessor csearch-results
            :initarg :results
            :documentation "Results of search.")
   (results-count :type fixnum
                  :accessor csearch-results-count
                  :initarg results-count
                  :initform 0
                  :documentation "Number of results.")
   (corpus :type corpus
           :accessor csearch-corpus
           :initarg :corpus
           :documentation "The corpus that was searched."))
  (:documentation
"The representation for a search of the corpus. Keeps track of both the search
and the results."))

(defgeneric make-csearch (corpus string regexp-p))
(defmethod make-csearch ((corpus corpus) (string string) regexp-p)
  (let ((this-search (make-instance 'csearch
                                    :string string
                                    :corpus corpus
                                    :results (make-array 0 :element-type 'line
                                                           :adjustable t
                                                           :fill-pointer 0)
                                    :regexp-p (if regexp-p t nil))))
    (loop :for f :across (corpus-files corpus)
          :do (loop :for l :across (file-lines f)
                    :when (and (data-line-p l)
                               (if (this-search 
                                   (cl-ppcre:scan string (line-contents l))
                                   (search string (line-contents l))))
                      :do (vector-push-extend l (csearch-results this-search))
                          (incf (csearch-results-count this-search))))
    (values this-search)))


  ;;
;;;;;; Search wrappers.
  ;;

(defgeneric search-corpus (corpus string)
  (:documentation
"Searches all files in the corpus CORPUS for a string STRING. Returns a
CSEARCH instance that contains a vector of all matching DATA-LINE instances in
the corpus."))

(defmethod search-corpus ((corpus corpus) (string string))
  (make-csearch corpus string nil))

(defgeneric re-search-corpus (corpus regexp)
  (:documentation
"Searches all files in the corpus CORPUS for a regular expression STRING.
Returns a CSEARCH instance that contains a vector of all matching DATA-LINE
instances in the corpus."))

(defmethod re-search-corpus ((corpus corpus) (regexp string))
  (make-csearch corpus regexp t))


  ;;
;;;;;; Presentation.
  ;;

(defgeneric present-search-results (search)
  (:documentation
"Present the results of a search in a human readable format."))

(defmethod present-search-results ((search csearch))
  (loop :for l :across (csearch-results search)
        :with count = 1               ;result counter
        :with lastfile                ;file of previous line
        :with lastpage                ;page of previous line
        :do (format t (concatenate 'string
                                   "~&~@[File: ~3,'0D~]~@[  Page: ~A~]"
                                   "~%~3D. ~A"
                                   "~%~5T~A"
                                   )
                    (if (or (null lastfile)
                            (not (equal lastfile (line-file l))))
                        (file-number (line-file l))
                        nil)
                    (if (or (null lastpage)
                            (and (slot-boundp l 'page)
                                 (not (equal lastpage (line-page l)))))
                        (line-page l)
                        nil)
                    count
                    (format-data-line-result l)
                    (format-trans-line-result (find-matching-trans-line l)))
            (setf count (1+ count)
                  lastfile (line-file l)
                  lastpage (if (slot-boundp l 'page) (line-page l) nil))))

(defmethod format-data-line-result ((line data-line))
  (format nil "~4D: ~A" (line-number line) (line-contents line)))

(defmethod format-trans-line-result ((line data-line))
  (format nil "~6T~A" (line-contents line)))

(defmethod find-matching-trans-line ((line data-line))
  (find-if (lambda (x) (and (data-line-p x)
                            (equal (line-number x) (line-number line))))
           (file-lines (entry-trans-file (file-entry (line-file line))))))

