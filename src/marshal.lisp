(defvar class-list (make-hash-table :test 'equal))
(defvar userdef-list (make-hash-table :test 'equal))

(defun decode-file (path &optional (package :cl-user))
  (with-open-file (file path :element-type '(unsigned-byte 8))
    (let ((major (read-byte file))
          (minor (read-byte file)))
      (when (or (/= major 4) (/= minor 8))
        (error "Marshal version ~A.~A is invalid." major minor))
      (decode file (make-hash-table :test #'equal) (make-hash-table :test #'equal)
              package))))

(defun add-class (ruby-name lisp-name)
  (setf (gethash ruby-name class-list) lisp-name))

(defun add-userdef (ruby-name lisp-name)
  (setf (gethash ruby-name userdef-list) lisp-name))

(defun decode (file syms objs package)
  (declare (type stream file))
  (ecase (read-byte-char file)
    (#\0 nil)
    (#\T 'true)
    (#\F 'false)
    (#\i (decode-integer file))
    (#\[ (decode-array file syms objs package))
    (#\{ (decode-hash file syms objs package nil))
    (#\} (decode-hash file syms objs package t))
    (#\: (decode-symbol file syms package))
    (#\; (decode-symlink file syms))
    (#\@ (decode-object-ref file objs))
    (#\I (decode-ivar file syms objs package))
    (#\" (decode-ascii file objs))
    (#\o (decode-object file syms objs package))
    (#\u (decode-userdef file syms objs package))))

(defgeneric userdef-decode (object len bytes))
(defgeneric userdef-encode (object))

(defun decode-userdef (file syms objs package)
  (let ((index (reserve-hash-index objs))
        (class (decode file syms objs package))
        (bytes (read-bytes file)))
    (let ((object (make-instance (find-lisp-userdef class))))
      (userdef-decode object (array-dimension bytes 0) bytes)
      (setf (gethash index objs) object))))

(defun find-lisp-userdef (symbol)
  (or (gethash (symbol-name symbol) userdef-list)
      (error "Userdef \"~A\" is not declared.~%Eval (~A \"~A\" 'SOME-CLASS-NAME)~%Then ~A and ~A must be implemented for that class." symbol 'add-userdef symbol 'userdef-decode 'userdef-encode)))

(defun find-lisp-class (symbol)
  (or (gethash (symbol-name symbol) class-list)
      (error "Class \"~A\" is not declared.~%Eval (~A \"~A\" 'SOME-CLASS-NAME)."
             symbol 'add-class symbol)))

(defun slot-char-p (char)
  (if (alpha-char-p char)
      (lower-case-p char)
      t))

(defun symbol->slot (symbol)
  (let ((name (symbol-name symbol)))
    (when (or (char/= (char name 0) #\@) (not (every #'slot-char-p name)))
      (error "Invalid Marshal data (unhandled slot name ~A)." symbol))
    (intern (substitute #\- #\_ (string-upcase (subseq name 1))))))

(defun decode-object (file syms objs package)
  (let ((index (reserve-hash-index objs))
        (class (decode file syms objs package)))
    (unless (symbolp class)
      (error "Invalid Marshal data (class name is not a symbol)."))
    (destructuring-bind (raw-keys values) (read-pairs file syms objs package)
      (let* ((lisp-class (find-lisp-class class))
             (object (make-instance lisp-class))
             (keys (mapcar #'symbol->slot raw-keys)))
        (anaphora:awhen (remove-if (lambda (key) (slot-exists-p object key)) keys)
          (error "Class ~A is lacking ~A." lisp-class anaphora:it))
        (loop for key in keys
           for value in values
           do (setf (slot-value object key) value))
        (setf (gethash index objs) object)))))

(defun read-bytes (file)
  (let* ((len (decode-integer file))
         (bytes (make-array len :element-type '(unsigned-byte 8))))
    (dotimes (i len)
      (setf (aref bytes i) (read-byte file)))
    bytes))

(defun decode-ascii (file objs)
  ;; I think it's not a good idea to solve invalid inputs returning them as a
  ;; byte sequence because "invalid" strings (e.g.: zlib data) may randomly be
  ;; stored as a "valid" string.
  (let ((bytes (read-bytes file)))
    (append-to-hash-and-return
     objs
     (handler-case (babel:octets-to-string bytes)
       ((or babel-encodings:invalid-utf8-starter-byte babel-encodings:end-of-input-in-character) ()
         bytes)))))

(defun decode-object-ref (file objs)
  (multiple-value-bind (value found) (gethash (decode-integer file) objs)
    (unless found
      (error "Invalid Marshal data (invalid object reference)."))
    (clone value)))

(defun decode-integer (file)
  (let ((raw (read-byte file)))
    (cond
      ((zerop raw) 0)
      ((<= 1 raw 4) (read-integer file raw))
      ((<= #xFC raw #xFF) (read-negative-integer file (1+ (- #xFF raw))))
      ((<= raw 127) (- raw 5))
      (t (- raw 251)))))

(defun clone (obj)
  (cond ((hash-table-p obj) (alexandria:copy-hash-table obj))
        ((stringp obj) (copy-seq obj))
        ((listp obj) (copy-tree obj))
        (t obj)))

(defun decode-symlink (file syms)
  (or (clone (gethash (decode-integer file) syms))
      (error "Invalid Marshal data (invalid symlink).")))

(defun reserve-hash-index (hash)
  (let ((index (hash-table-count hash)))
    (setf (gethash index hash) nil)
    index))

(defun append-to-hash-and-return (hash value)
  (setf (gethash (hash-table-count hash) hash) value)
  value)

(defun decode-symbol (file syms package)
  (let* ((bytes (decode-integer file))
         (name (trivial-utf-8:read-utf-8-string file :byte-length bytes))
         (symbol (intern name package)))
    (append-to-hash-and-return syms symbol)))

(defun decode-array (file syms objs package)
  (let ((index (reserve-hash-index objs))
        (list (loop for i from 0 upto (1- (decode-integer file))
                 collect (decode file syms objs package))))
    (setf (gethash index objs) list)))

(defun decode-ivar (file syms objs package)
  (let ((index (reserve-hash-index objs))
        (mode (read-byte-char file))
        (bytes (read-bytes file)))
    ;; for now, ignore key values
    (read-pairs file syms objs package)
    (setf (gethash index objs)
          (ecase mode
            (#\" (trivial-utf-8:utf-8-bytes-to-string bytes))
            (#\/ (error "Regex are not implemented (yet®)."))))))

(defun read-pairs (file syms objs package)
  (loop with keys = nil
     with values = nil
     for i from 0 upto (1- (decode-integer file))
     do (push (decode file syms objs package) keys)
     do (push (decode file syms objs package) values)
     finally (return-from read-pairs
               (list (nreverse keys) (nreverse values)))))

(defun decode-hash (file syms objs package has-default)
  (when has-default (error "Default hash not implemented."))
  (let* (;;(default (if has-default (decode file syms objs) nil))
         (hash (make-hash-table :test #'equal)))
    (loop with (keys values) = (read-pairs file syms objs package)
       for key in keys
       for value in values
       do (setf (gethash key hash) value))
    (append-to-hash-and-return objs hash)))

(defun read-typecode (file)
  (read-byte file))

(defun read-integer (file bytes)
  (loop for i from 0 to (1- bytes)
     for component = (ash (read-byte file) (bytes->bits i))
     summing component into integer
     finally (return integer)))

(defun read-negative-integer (file bytes)
  (- (read-integer file bytes)
     (bytes->power2 bytes)))

(defun bytes->power2 (bytes)
  (declare (type integer bytes))
  (ash 1 (bytes->bits bytes)))

(defun bytes->bits (n)
  (declare (type integer n))
  (* n 8))

(defun switch-char (char)
  (cond ((upper-case-p char) (char-downcase char))
        ((lower-case-p char) (char-upcase char))
        (t char)))

(defun read-byte-char (stream)
  (code-char (read-byte stream)))
