;;;; -*- Mode: Common-Lisp -*-
;;;;
;;;; Neek: Tlingit corpus utility.
;;;;
;;;; Corpus lines.
;;;;
;;;; A corpus line is different from a raw (file) line. A raw line is just
;;;; some hunk of text in a file delimited by a newline character. A corpus
;;;; line is a line from a corpus entry file that contains data, metadata,
;;;; whitespace, is empty, or is ‘other’. Raw lines are just strings whereas
;;;; corpus lines are instances of one of the subclasses of the LINE class. A
;;;; raw line must be parsed into a corpus line; see line-parsing.lisp for
;;;; those operations.

(in-package "NEEK")

(defclass line ()
  ((raw    :type string
           :accessor line-raw
           :initarg :raw
           :documentation "Raw, unparsed line from file.")
   (length :type fixnum
           :accessor line-length
           :initarg :length
           :documentation "Length of raw line in characters.")
   (posn   :type fixnum
           :accessor line-posn
           :initarg :index
           :documentation "Position (raw line number) in file.")
   (page   :type string
           :accessor line-page
           :initarg :page
           :documentation "Page number (string!) in file.")
   (para   :type string
           :accessor line-para
           :initarg :para
           :documentation "Paragraph number (string!) in file.")
   (prev   :type line                   ;this is circular but works so far
           :accessor line-prev
           :initarg :prev
           :documentation "Preceding line in file.")
   (next   :type line                   ;idem
           :accessor line-next
           :initarg :next
           :documentation "Next line in file.")
   (file   :accessor line-file          ;no type because FILE isn’t defined yet
           :documentation "File containing this line."))
   (:documentation
"Representation of a line in a corpus file. This is a base class from which
various subclasses derive. After fully reading and parsing the contents of a
file into a FILE instance, there should only be instances of subclasses of
this class and no instances of this base class."))

(defclass empty-line (line) ()
  (:documentation
"Representation of an empty line in a corpus file. Normally there should not
be empty lines, so instances of this class indicate that the file may have
malformed structure."))

(defclass whitespace-line (line) ()
  (:documentation
"Representation of an 'empty' line in a corpus file that actually contains
only whitespace characters. Such lines should not normally exist so instances
of this class indicate that the file may have malformed structure."))

(defclass other-line (line) ()
  (:documentation
"Representation for a line in a corpus file that was not successfully
identified as a data line, a metadata line, a special line, a whitespace line,
or an empty line. Instances of this class indicate that the file has malformed
structure."))

(defclass data-line (line)
  ((number   :type fixnum
             :accessor line-number
             :initarg :number
             :documentation "Corpus line number.")
   (contents :type string
             :accessor line-contents
             :initarg :contents
             :documentation "Contents of corpus line without number.")
   (words    :type (vector string *)
             :accessor line-words
             :initarg :words
             :documentation "Orthographic words as a vector of strings."))
  (:documentation
"Representation of a line containing data (text) in a corpus file. The NUMBER
slot contains the line number from the text. The CONTENTS slot contains the
actual textual content of the line."))

;;; TODO: Decide whether these should be erroneous or acceptable.
(defclass blank-data-line (data-line) ()
  (:documentation
"Representation of a line containing data (text) in a corpus file. Like a
DATA-LINE but the contents of the line are either empty or whitespace. Not
actually an EMPTY-LINE or WHITESPACE-LINE because there is a valid data line
number; it is only the contents that are empty."))

;;; TODO: Plan for different kinds of special data. Right now the only stigma
;;; is "*" for Tagged files, but the Gloss files should be converted to use
;;; another stigma (e.g. "$"). Then this class could be further subclassed
;;; for each type of special line.
(defclass special-data-line (data-line)
  ((stigma :type string
           :accessor special-stigma
           :initarg :stigma
           :documentation "Character appearing after the line number.")))

;;; FIXME: Make use of this.
(defclass blank-special-data-line (special-data-line blank-data-line) ())

;;; FIXME: Develop subclasses for different kinds of metadata that describe
;;; names, dates, sources, notes, pages, comments, etc. PAGE-META-LINE would
;;; be useful to point back to from following data lines. COMMENT-META-LINE
;;; could have its VALUE slot parsed to determine what data line the comment
;;; refers to since it usually starts out with "Line XXX is/has…".
(defclass meta-line (line)
  ((key   :type string
          :accessor meta-key
          :initarg :key
          :documentation "Key of metadatum key/value pair.")
   (value :type string
          :accessor meta-value
          :initarg :value
          :documentation "Value of metadatum key/value pair."))
  (:documentation
"Representation of a line containing metadata in a corpus file. Metadata lines
are structured as key/value pairs."))


  ;;
;;;;;; Line parsing.
  ;;

(defgeneric find-first-tab (l)
  (:documentation
"Returns the position of the first tab in a line. This first tab separates the
line number from the contents in a data line. Returns NIL if there are no tabs
in the line, implying that it is not a data line."))

(defmethod find-first-tab ((l string))
  (position #\Tab l))

(defgeneric find-first-brace (l)
  (:documentation
"Returns the position of the first brace in a line. If the brace is the first
character in the line then this is a metadata line."))

(defmethod find-first-brace ((l string))
  (position #\{ l))

(defgeneric find-last-brace (l)
  (:documentation
"Returns the position of the last brace in a line. This last brace should occur
at the end of a metadata line."))

(defmethod find-last-brace (l)
  (position #\} l :from-end t))

(defgeneric find-first-equalsign (l)
  (:documentation
"Returns the position of the first = in a line. This separates the key from
the value in a metadata line."))

(defmethod find-first-equalsign ((l string))
  (position #\= l))

(defgeneric parse-line-meta (l len))
(defmethod parse-line-meta ((l string) (len fixnum))
  (let ((eqpos (find-first-equalsign l))
        (cbpos (find-last-brace l)))
    ;; Some crude syntax checking to ensure parsing sanity.
    ;; FIXME: Should report the file and line number in these
    ;; errors/warnings. Signal a condition and handle it later.
    (if (null eqpos) (error "Meta line missing =."))
    (if (null cbpos) (error "Meta line missing closing brace."))
    (if (not (equal cbpos (1- len)))
        (warn "Meta line has junk after closing brace."))
    (values (string-trim +whitespace+ (subseq l 1 eqpos))
            (string-trim +whitespace+ (subseq l (1+ eqpos) cbpos)))))

(defgeneric parse-line (l)
  (:documentation
"Returns the various parts of a line as multiple values. The first value is
always a symbol representing the type of line: EMPTY-LINE, WHITESPACE-LINE,
META-LINE, DATA-LINE, BLANK-DATA-LINE, SPECIAL-DATA-LINE, or OTHER-LINE. The
second value is always the length of the raw line in characters; this is > 0
for all but EMPTY-LINEs. The remaining values depend on the type of line,
i.e. the first value. If EMPTY-LINE, WHITESPACE-LINE, or OTHER-LINE then there
are no other values. If META-LINE then the third and fourth values are the key
and value of the key/value metadata pair. If DATA-LINE, BLANK-DATA-LINE, or
SPECIAL-DATA-LINE then the third and fourth values are the line number and the
contents of the line. If SPECIAL-DATA-LINE then there is also a fifth value
which contains the stigma that follows the line number."))

;;; FIXME: Break this up into smaller pieces. It’s too long.
(defmethod parse-line ((l string))
  (let ((len (length l)))
    (if (zerop len)
        ;; Empty.
        (values 'empty-line len)
        ;; Maybe whitespace, data, special, meta, or other.
        (if (zerop (length (string-trim +whitespace+ l)))
            ;; Whitespace.
            (values 'whitespace-line len)
            ;; Not whitespace; maybe data, special, meta, or other.
            (if (equal (char l 0) #\{)
                ;; Meta. Get its parts.
                (multiple-value-bind (key val) (parse-line-meta l len)
                  (values 'meta-line len key val))
                ;; Not meta; maybe data, special, or other.
                (let ((tabspot (find-first-tab l)))
                  (if (or (null tabspot) (zerop tabspot))
                      ;; Other: no tab, or has tab but no line number.
                      (values 'other-line len)
                      ;; Has tab and stuff before it; maybe special,
                      ;; blank-data, data, or other.
                      (let ((pretab (subseq l 0 tabspot)))
                        (multiple-value-bind (linum junk)
                            (parse-integer pretab :junk-allowed t)
                          (if (integerp linum)
                              ;; Has line number; maybe data, blank-data, or
                              ;; special.
                              (if (> (length pretab)
                                     (length (prin1-to-string linum)))
                                  ;; Has stuff after line number so special.
                                  (values 'special-data-line len linum
                                          (subseq l junk tabspot))
                                  ;; Maybe data or blank-data.
                                  (let ((posttab (subseq l (1+ tabspot))))
                                    (if (zerop
                                         (length
                                          (string-trim +whitespace+ posttab)))
                                        ;; Nothing after tab so blank-data.
                                        (values 'blank-data-line len linum posttab)
                                        ;; Data.
                                        (values 'data-line len linum posttab))))
                              ;; Elsewhere case.
                              (values 'other-line len)))))))))))

(defgeneric parse-data-words (l))
(defmethod parse-data-words ((l string))
  (split-sequence:split-sequence #\Space l))

(defgeneric make-line (l))

(defmethod make-line ((l string))
  (multiple-value-bind (type len three four five) (parse-line l)
    (case type
      ((empty-line whitespace-line other-line)
       (make-instance type :raw l :length len))
      (meta-line
       (make-instance type :raw l :length len
                           :key three :value four))
      (data-line
       (make-instance type :raw l :length len
                           :number three :contents four
                           :words (parse-data-words four)))
      (special-data-line
       (make-instance type :raw l :length len
                           :number three :contents four
                           :stigma five))
      (t (error "Unknown line type. Can't happen.")))))


  ;;
;;;;;; Predicates.
  ;;

;;; The …-LINE-P methods all use CLOS dispatch to determine type. They all
;;; accept any instance of LINE or its subclasses, but they’ll barf on
;;; anything else so don’t feed them strings.
(defgeneric meta-line-p (l))
(defmethod meta-line-p ((l meta-line)) t)
(defmethod meta-line-p ((l line)) nil)

(defgeneric data-line-p (l))
(defmethod data-line-p ((l data-line)) t)
(defmethod data-line-p ((l line)) nil)

(defgeneric blank-data-line-p (l))
(defmethod blank-data-line-p ((l blank-data-line)) t)
(defmethod blank-data-line-p ((l line)) nil)

(defgeneric special-data-line-p (l))
(defmethod special-data-line-p ((l special-data-line)) t)
(defmethod special-data-line-p ((l line)) nil)

(defgeneric empty-line-p (l))
(defmethod empty-line-p ((l empty-line)) t)
(defmethod empty-line-p ((l line)) nil)

(defgeneric whitespace-line-p (l))
(defmethod whitespace-line-p ((l whitespace-line)) t)
(defmethod whitespace-line-p ((l line)) nil)

(defgeneric other-line-p (l))
(defmethod other-line-p ((l other-line)) t)
(defmethod other-line-p ((l line)) nil)
