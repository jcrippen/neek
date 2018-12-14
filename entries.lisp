;;;; -*- Mode: Common-Lisp -*-
;;;;
;;;; Neek: Tlingit corpus utility.
;;;;
;;;; Corpus entries.

(in-package "NEEK")

(defclass entry ()
  ((number     :type fixnum
               :accessor entry-number
               :initarg :number)
   (numstr     :type string
               :accessor entry-numstr
               :initarg :numstr)
   (title      :type string
               :accessor entry-title
               :initarg :title)
   (author     :type string
               :accessor entry-author
               :initarg :author)
   (translator :type string
               :accessor entry-translator
               :initarg :translator)
   (text-file  :type text-file
               :accessor entry-text-file
               :initarg :text-file)
   (trans-file :type trans-file
               :accessor entry-trans-file
               :initarg :trans-file)
   (orig-file  :type orig-file
               :accessor entry-orig-file
               :initarg :orig-file)
   (gloss-file :type gloss-file
               :accessor entry-gloss-file
               :initarg :gloss-file)
   (tag-file   :type tag-file
               :accessor entry-tag-file
               :initarg :tag-file)))

(defgeneric add-file-to-entry (f e)
  (:documentation
"Adds a file F to a corpus entry E. Returns the file. Will not overwrite an
existing file already stored in E. After adding F, the metadata slots in E are
updated with information from F. The ENTRY slot in F is also set to E."))

;;; FIXME: Make the SIMPLE-ERRORs below more specific.
(defmethod add-file-to-entry ((f text-file) (e entry))
  (if (slot-boundp e 'text-file)
      (error "Can’t add ~S because TEXT-FILE is already bound in ~S" f e)
      (setf (entry-text-file e) f)))

(defmethod add-file-to-entry ((f trans-file) (e entry))
  (if (slot-boundp e 'trans-file)
      (error "Can’t add ~S because TRANS-FILE is already bound in ~S" f e)
      (setf (entry-trans-file e) f)))

(defmethod add-file-to-entry ((f orig-file) (e entry))
  (if (slot-boundp e 'orig-file)
      (error "Can’t add ~S because ORIG-FILE is already bound in ~S" f e)
      (setf (entry-orig-file e) f)))

(defmethod add-file-to-entry ((f gloss-file) (e entry))
  (if (slot-boundp e 'gloss-file)
      (error "Can’t add ~S because GLOSS-FILE is already bound in ~S" f e)
      (setf (entry-gloss-file e) f)))

(defmethod add-file-to-entry ((f tag-file) (e entry))
  (if (slot-boundp e 'tag-file)
      (error "Can’t add ~S because TAG-FILE is already bound in ~S" f e)
      (setf (entry-tag-file e) f)))

(defmethod add-file-to-entry :after ((f file) (e entry))
  ;; FIXME: Repetitive. Should have something that maps over a list of
  ;; slots. Could be a list of conses where the car is a symbol for a slot in
  ;; E and cdr a symbol for a slot in F.
  (unless (slot-boundp e 'number)
    (setf (entry-number e) (file-number f)))
  (unless (slot-boundp e 'numstr)
    (setf (entry-number e) (file-numstr f)))
  (unless (slot-boundp e 'author)
    (setf (entry-author e) (file-author f)))
  (unless (slot-boundp e 'title)
    (setf (entry-title e) (file-title f)))
  (unless (slot-boundp f 'entry)
    (setf (file-entry f) e)))

(defmethod add-file-to-entry :after ((f trans-file) (e entry))
  (unless (slot-boundp e 'translator)
    (setf (entry-translator e) (file-translator f))))

(defgeneric make-entry-from-file (f)
  (:documentation
"Creates an instance of ENTRY based on the properties of the file F. Returns
the new ENTRY instance. Does not store the file in the entry."))

(defmethod make-entry-from-file ((f file))
  (make-instance 'entry
                 :number (file-number f)
                 :numstr (file-numstr f)
                 :title  (file-title f)
                 :author (file-author f)))

(defgeneric file-in-entry-p (f e)
  (:documentation
"Returns T if the file F is in the entry E."))

(defmethod file-in-entry-p ((f text-file) (e entry))
  (or (equalp f (entry-text-file e))
      (and (equalp (file-number f) (file-number (entry-text-file e)))
           (equalp (file-numstr f) (file-numstr (entry-text-file e))))))

(defmethod file-in-entry-p ((f trans-file) (e entry))
  (or (equalp f (entry-trans-file e))
      (and (equalp (file-number f) (file-number (entry-trans-file e)))
           (equalp (file-numstr f) (file-numstr (entry-trans-file e))))))

(defmethod file-in-entry-p ((f orig-file) (e entry))
  (or (equalp f (entry-orig-file e))
      (and (equalp (file-number f) (file-number (entry-orig-file e)))
           (equalp (file-numstr f) (file-numstr (entry-orig-file e))))))

(defmethod file-in-entry-p ((f gloss-file) (e entry))
  (or (equalp f (entry-gloss-file e))
      (and (equalp (file-number f) (file-number (entry-gloss-file e)))
           (equalp (file-numstr f) (file-numstr (entry-gloss-file e))))))

(defmethod file-in-entry-p ((f tag-file) (e entry))
  (or (equalp f (entry-tag-file e))
      (and (equalp (file-number f) (file-number (entry-tag-file e)))
           (equalp (file-numstr f) (file-numstr (entry-tag-file e))))))

(defgeneric file-matches-entry-p (f e)
  (:documentation
"Returns T if the file F should be stored in (belongs to) entry E.
This does not check if F is actually stored in E, but rather whether the
metadata of E and F match so that F should be one of the files of E."))

(defmethod file-matches-entry-p ((f file) (e entry))
  (and (equalp (file-number f) (entry-number e))
       (equalp (file-numstr f) (entry-numstr e))))
