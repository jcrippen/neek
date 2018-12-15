;;;; -*- Mode: Common-Lisp -*-
;;;;
;;;; Neek: Tlingit corpus utility.
;;;;
;;;; Corpus definition and top level.

(in-package "NEEK")

(defvar *current-corpus* nil)

(defclass corpus ()
  ((entries :type (vector entry *)
            :initarg :entries
            :documentation "A vector of corpus entries.")
   (entnum  :type fixnum
            :initarg :entnum
            :documentation "Number of entries in the corpus.")
   (files   :type (vector file *)
            :initarg :files
            :documentation "A vector of files in the corpus.")
   (filenum :type fixnum
            :initarg :filenum
            :documentation "Number of files in the corpus."))
  (:documentation
   "The top-level object that contains a corpus. The ENTRIES slot contains a
vector of ENTRY instances, one for each entry in the corpus."))

(defgeneric load-corpus (dir)
  (:documentation
"Load a corpus in the directory DIR. Returns a CORPUS instance containing
ENTRY instances for each entry in the corpus."))

(defmethod load-corpus ((dir string))
  (load-corpus (uiop:merge-pathnames* dir)))

(defmethod load-corpus ((dir pathname))
  (let ((flist (mapcar #'make-file (list-corpus-files dir)))
        (files (make-array 0 :element-type 'file
                             :adjustable t
                             :fill-pointer 0))
        (filenum 0)
        (entnum 0)
        (entries (make-array 0 :element-type 'entry
                               :adjustable t
                               :fill-pointer 0)))
    ;; FIXME: Better error.
    (if (null files) (error "No corpus files found."))
    ;; CDR down the files list, storing each one in the file vector and adding
    ;; it to an entry or making a new entry for it as appropriate.
    (loop :for f :in flist
          :with ent                     ;current entry
          :when (not (null f))
            :do
               (vector-push-extend f files)
               (incf filenum)
          :until (null f)
            :do
             (if (null ent)
                 ;; First time so make a new entry.
                 (progn (setf ent (make-entry-from-file f))
                        (incf entnum)
                        (vector-push-extend ent entries))
                 (if (file-matches-entry-p f ent)
                     ;; Add to current entry.
                     (add-file-to-entry f ent)
                     ;; Otherwise find a matching entry.
                     (let ((match (find-if (lambda (x)
                                             (file-matches-entry-p f x))
                                           entries :from-end t)))
                       (if (not (null match))
                           ;; An earlier entry matches, so add file to it.
                           (add-file-to-entry f match)
                           ;; No matches so make a new entry from this file.
                           (progn (setf ent (make-entry-from-file f))
                                  (incf entnum)
                                  (vector-push-extend ent entries)))))))
    ;; Now make a CORPUS instance and return it.
    (values
     (make-instance 'corpus
                    :entries entries
                    :entnum entnum
                    :files files
                    :filenum filenum))))
