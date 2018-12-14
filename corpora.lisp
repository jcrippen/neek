;;;; -*- Mode: Common-Lisp -*-
;;;;
;;;; Neek: Tlingit corpus utility.
;;;;
;;;; Corpus definition and top level.

(in-package "NEEK")

(defclass corpus ()
  ((entries :type (vector entry *)
            :documentation "A vector of corpus entries.")
   (entnum  :type fixnum
            :documentation "Number of entries in ENTRIES slot."))
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
  (let ((files (mapcar #'make-file (list-corpus-files dir)))
        (entries (make-array 0 :element-type 'entry
                               :adjustable t
                               :fill-pointer 0)))
    ;; FIXME: Better error.
    (if (null files) (error "No corpus files found."))
    ;; Cdr down the files, making entries and storing files in them as
    ;; appropriate.
    (loop :for f :in files
          :with ent                     ;current entry
          :until (null f)
          :do
             (if (null ent)
                 ;; First time so make a new entry.
                 (progn (setf ent (make-entry-from-file f))
                        (vector-push-extend ent entries))
                 (if (file-matches-entry-p f ent)
                     ;; Add to current entry.
                     (add-file-to-entry f ent)
                     (let ((match (find-if (lambda (x)
                                             (file-matches-entry-p f x))
                                           entries :from-end t)))
                       (if (not (null match))
                           ;; Earlier entry matches, so add file to it.
                           (add-file-to-entry f match)
                           ;; No matches so make a new entry from this file.
                           (progn
                             (setf ent (make-entry-from-file f))
                             (vector-push-extend ent entries)))))))
    (values entries)))
