;;;; -*- Mode: Common-Lisp -*-
;;;;
;;;; Neek: Tlingit corpus utility.
;;;;
;;;; Corpus files.

(in-package "NEEK")

;;; FIXME: This number is magic. Calculate a reasonable minimum empirically,
;;; with the goal of not having to grow a vector too much, but not wasting
;;; lots of space with huge empty vectors. Should be no bigger than the
;;; minimum size of any file in the corpus.
(defvar %init-num-lines% 50
  "Initial size of a LINES vector in a FILE object.")

;;; Note: The accessor methods can collide with other commonly used FILE-…
;;; symbols. For example, in ‘package.lisp’ we shadow FILE-AUTHOR which is in
;;; the COMMON-LISP package.
(defclass file ()
  ((pathname  :type pathname
              :accessor file-pathname
              :initarg :pathname
              :documentation "Lisp pathname object.")
   (number    :type fixnum
              :accessor file-number
              :initarg :number
              :documentation "Corpus entry number.")
   (numstr    :type string
              :accessor file-numstr
              :initarg :numstr
              :documentation "Corpus entry number as string from file.")
   (title     :type string
              :accessor file-title
              :initarg :title
              :documentation "Entry name from file.")
   (author    :type string
              :accessor file-author
              :initarg :author
              :documentation "Author of entry.")
   (size      :type fixnum
              :accessor file-size
              :initarg :size
              :documentation "File size in bytes (not characters).")
   (linecount :type fixnum
              :accessor file-linecount
              :initarg :linecount
              :documentation "Number of lines.")
   (datacount :type fixnum
              :accessor file-datacount
              :initarg :datacount
              :documentation "Number of data lines.")
   (datastart :type fixnum
              :accessor file-datastart
              :initarg :datastart
              :documentation "Position of first data line.")
   (lines     :type (vector line *)
              :accessor file-lines
              :initarg :lines
              :documentation "Vector of corpus lines.")
   (entry     :accessor file-entry      ;no type because ENTRY isn’t defined yet
              :documentation "Entry containing this file.")))

(defclass orthographic ()
  ((orthography :type string
                :accessor file-orthography
                :initarg :orthography
                :documentation "Orthography used in data lines."))
  (:documentation
   "Mixin class for files that contain material in an orthography."))

(defclass text-file (file orthographic) ())

(defclass orig-file (file orthographic) ())

(defclass gloss-file (file orthographic) ())

(defclass tag-file (file orthographic) ())

(defclass trans-file (file orthographic)
  ((translator :type string
               :accessor file-translator
               :initarg :translator
               :documentation "Translator of entry.")))


  ;;
;;;;;; Reading files.
  ;;

;;; FIXME: Rework to use Flexi-Streams for portability where the option :UTF-8
;;; for :EXTERNAL-FORMAT is unsupported. See CL-Unicode’s WITH-UNICODE-FILE in
;;; ‘build/read.lisp’ for example. But :UTF-8 works on SBCL, CCL, ECL,
;;; CLISP, and LispWorks at least.
(defmacro with-corpus-file ((stream path) &body body)
  `(with-open-file (,stream ,path :direction :input
                                  :if-does-not-exist :error
                                  :external-format :utf-8)
     ,@body))

(defgeneric string-to-corpus-pname (str)
  (:documentation
"Converts a filename as a string into a corpus pathname. The argument STR is
concatenated with *CORPUS-DIR* and then made into a pathname."))

;;; FIXME: Don’t use *CORPUS-DIR*. Instead pass the base directory as an
;;; argument, and use *CORPUS-DIR* just for testing.
(defmethod string-to-corpus-pname ((str string))
  (uiop:merge-pathnames* (concatenate 'string *corpus-dir* str)))

(defgeneric corpus-file-p (arg)
  (:documentation
"Returns T if ARG represents a file that contains corpus data. The ARG can be
a string, a pathname, or a FILE instance. If it is a string or a pathname then
its contents are tested, but if it is a FILE instance (or a subclass thereof)
then this predicate is true by definition."))

(defmethod corpus-file-p ((arg string))
  (corpus-file-p (string-to-corpus-pname arg)))

(defmethod corpus-file-p ((arg pathname))
  (with-corpus-file (s arg)
    ;; This is a pretty crappy test. It checks if the first line starts with
    ;; an open brace and ends with a close brace. Basically like Unix
    ;; file(1). But this test is fast and doesn’t parse the whole thing.
    (let ((lineone (read-line s nil)))
      (and (equal #\{ (char lineone 0))
           (equal #\} (char lineone (1- (length lineone))))))))

(defmethod corpus-file-p ((f file)) t)  ;true by definition

(defgeneric read-file (f)
  (:documentation
"Reads a file F to return three values: a vector of LINES, one for each line,
a total count of those lines, and size of the file in bytes. These are used by
MAKE-FILE which creates the appropriate FILE subclass instance. If the
argument F is a string then it is converted to a pathname with STRING-TO-
CORPUS-PNAME. The returned vector may be longer than the actual number of
lines so its content length is tracked separately. The file on disk is only
looked at inside of this method and nowhere else so all filesystem metadata
must be obtained through this method."))

(defmethod read-file ((f string))
  (read-file (string-to-corpus-pname f)))

(defmethod read-file ((f pathname))
  (when (not (corpus-file-p f))
    (error "Not a corpus file."))       ;FIXME: Better error.
  (let ((lines (make-array %init-num-lines%
                           :element-type 'line
                           :adjustable t
                           :fill-pointer 0))
        (count 0)
        (size 0))
    (with-corpus-file (s f)
      (loop :for line = (cl:read-line s nil)
            :for index :from 1
            :until (null line)
            :do (vector-push-extend (make-line line) lines)
            :finally (setf count index))
      (setf size (cl:file-length s)))
    (values lines count size)))


  ;;
;;;;;; Basic line finding tools.
  ;;

(defgeneric get-header-lines (f)
  (:documentation
"Returns the header metadata lines from F. The header metadata lines are those
META-LINE instances that appear before the first DATA-LINE instance. Any Page
or Paragraph elements preceding the DATA-LINE are also excluded."))

(defmethod get-header-lines ((f vector))
  (loop :for l :across v
        :until (not (meta-line-p l))
        :when (not (or (equal (meta-key l) "Page")
                       (equal (meta-key l) "Paragraph")))
          :collect l))

(defmethod get-header-lines ((f file))
  (get-header-lines (file-lines f)))

(defgeneric get-header-meta-value (f k)
  (:documentation
"Returns the value of the first matching META-LINE with the key K in the
header lines of F. The key K must be a string."))

(defmethod get-header-meta-value ((f vector) (k string))
  (loop :for l :across f
        :when (equalp (meta-key l) k)
          :return (meta-value l)))

(defmethod get-header-meta-value ((f file) (k string))
  (get-header-meta-value (get-header-lines f) k))


  ;;
;;;;;; Making FILEs.
  ;;

(defgeneric make-file (f)
  (:documentation
"Creates an instance of a FILE subclass from the file F. If the argument F is
a string it is converted to a pathname with STRING-TO-CORPUS-PNAME. The file
is read in using READ-FILE and the instance is filled in with its array of
LINEs and other metadata extracted from that array."))

(defmethod make-file ((f string))
  (make-file (string-to-corpus-pname f)))

(defmethod make-file ((f pathname))
  (multiple-value-bind (lines count size) (read-file f)
    (let* ((ftype (get-header-meta-value lines "Type"))
           (file (make-instance (string-case:string-case (ftype)
                                  ("Text"        'text-file)
                                  ("Translation" 'trans-file)
                                  ("Original"    'orig-file)
                                  ("Gloss"       'gloss-file)
                                  ("Tagged"      'tag-file)
                                  (t             'file)) ;shouldn't happen
                                :pathname f
                                :lines lines
                                :linecount count
                                :size size)))
      ;; Iterate through the lines and set various FILE slots.
      (loop :for l :across lines
            :for lcnt :from 0           ;count of lines
            :with dcnt = 0              ;count of data lines
            :with prevline              ;the previous line
            :with pagenum               ;current page number
            :with paranum               ;current paragraph number
            :until (null l)
            :do ;; Slots that depend on the line’s type.
               (cond
                 ((meta-line-p l)
                  (string-case:string-case ((meta-key l))
                    ("Number" (setf (file-numstr file) (meta-value l))
                              (setf (file-number file) (parse-integer (meta-value l))))
                    ("Title"  (setf (file-title  file) (meta-value l)))
                    ("Author" (setf (file-author file) (meta-value l)))
                    ("Translator" (setf (file-translator file) (meta-value l)))
                    ("Page"      (setf pagenum (meta-value l)))
                    ("Paragraph" (setf paranum (meta-value l)))
                    ("Orthography" (setf (file-orthography file) (meta-value l)))
                    ;; Other metadata are ignoreable here.
                    (t nil)))
                 ((data-line-p l)
                  ;; If this is the first data line then set DATASTART.
                  (if (zerop dcnt) (setf (file-datastart file) lcnt))
                  ;; Bump the data line counter.
                  (setf dcnt (1+ dcnt))))
               ;; Other slots depend on context.
               (unless (null prevline) (setf (line-prev l) prevline))
               (unless (null pagenum)  (setf (line-page l) pagenum))
               (unless (null paranum)  (setf (line-para l) paranum))
               (setf (line-posn l) lcnt
                     (line-file l) file)
               ;; And remember the previous line for the next iteration.
               (setf prevline l)
            :finally
               (setf (file-linecount file) lcnt
                     (file-datacount file) dcnt))
      ;; Now iterate in reverse over the lines and set their NEXT slots.
      (loop :for i :downfrom (1- (file-linecount file)) :to 0
            :for l = (aref lines i)
            :with nextline
            :do (unless (null nextline) (setf (line-next l) nextline))
                (setf nextline l))
      (values file))))
