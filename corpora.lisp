;;;; -*- Mode: Common-Lisp -*-
;;;;
;;;; Neek: Tlingit corpus utility.
;;;;
;;;; Corpus definition and top level.

(in-package "NEEK")

(defvar *current-corpus* nil)

(defclass corpus ()
  ((entries :type (vector entry *)
            :accessor corpus-entries
            :initarg :entries
            :documentation "A vector of corpus entries.")
   (entnum  :type fixnum
            :accessor corpus-entnum
            :initarg :entnum
            :documentation "Number of entries in the corpus.")
   (files   :type (vector file *)
            :accessor corpus-files
            :initarg :files
            :documentation "A vector of files in the corpus.")
   (filenum :type fixnum
            :accessor corpus-filenum
            :initarg :filenum
            :documentation "Number of files in the corpus."))
  (:documentation
   "The top-level object that contains a corpus. The ENTRIES slot contains a
vector of ENTRY instances, one for each entry in the corpus. The FILES slot
contains a vector of FILE instances, one for each file in the corpus. The
FILES slot is an optimization because many operations on the corpus search
through files rather than entries, but the user does not need to know this."))

(defgeneric make-corpus (dir)
  (:documentation
"Make a corpus from files in the directory DIR. Returns a CORPUS instance that
contains ENTRY instances for each entry in the corpus and FILE instances for
each file in the corpus."))

(defmethod make-corpus ((dir string))
  (load-corpus (uiop:merge-pathnames* dir)))

(defmethod make-corpus ((dir pathname))
  (let ((flist (mapcar #'make-file (list-corpus-files dir)))
        (files (make-array 0 :element-type 'file
                             :adjustable t
                             :fill-pointer 0))
        (entries (make-array 0 :element-type 'entry
                               :adjustable t
                               :fill-pointer 0))
        (filenum 0)
        (entnum 0))
    ;; FIXME: Better error.
    (if (null flist) (error "No corpus files found."))
    ;; CDR down the files list, either making a new entry for it
    ;; or adding it to an existing entry as appropriate. Then store
    ;; the file in the file vector FILES.
    (loop :for f :in flist
          :with ent = nil               ;current entry
          :until (null f)
          :do
             (if (null ent)
                 ;; First iteration so make a new entry.
                 (progn (setf ent (make-entry-from-file f))
                        (add-file-to-entry f ent)
                        (incf entnum)
                        (vector-push-extend ent entries))
                 (if (file-matches-entry-p f ent)
                     ;; Add the file to the current entry.
                     (add-file-to-entry f ent)
                     ;; Otherwise find an earlier entry that matches.
                     (let ((match (find-if (lambda (x)
                                             (file-matches-entry-p f x))
                                           entries :from-end t)))
                       (if (not (null match))
                           ;; An earlier entry matches, so add the file to it.
                           (add-file-to-entry f match)
                           ;; No matches so make a new entry from this file.
                           (progn (setf ent (make-entry-from-file f))
                                  (add-file-to-entry f ent)
                                  (incf entnum)
                                  (vector-push-extend ent entries))))))
             ;; Finally record the file.
             (progn (vector-push-extend f files)
                    (incf filenum)))
    ;; Now make a CORPUS instance and return it.
    (make-instance 'corpus
                    :entries entries
                    :entnum entnum
                    :files files
                    :filenum filenum)))
