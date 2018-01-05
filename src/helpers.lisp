(in-package :marshal)

(defun clone (obj)
  (cond ((hash-table-p obj) (copy-hash-table obj))
        ((stringp obj) (copy-seq obj))
        ((listp obj) (copy-tree obj))
        (t obj)))

(defun reserve-hash-index (hash)
  (let ((index (hash-table-count hash)))
    (setf (gethash index hash) nil)
    index))

(defun append-to-hash-and-return (hash value)
  (setf (gethash (hash-table-count hash) hash) value)
  value)

(defun read-integer (file bytes)
  ;; TODO test in big-endian
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

(defun valid-lisp-symbol-p (symbol)
  (let ((name (symbol-name symbol)))
    (and (string= (string-upcase name) name)
         (notany (lambda (char) (char= char #\_)) name))))

(defun valid-ruby-symbol-p (symbol-name)
  (flet ((hyphen-p (char)
           (char= char #\-)))
    (notany #'hyphen-p symbol-name)))

(defun symbol->slot (symbol)
  (let ((name (symbol-name symbol)))
    (when (char/= (char name 0) #\@)
      (error "Invalid Marshal data (unhandled slot name ~A)." symbol))
    (intern (subseq name 1) (symbol-package symbol))))

(defun ruby-symbol->lisp-symbol (symbol-name package)
  (if (or (ruby-class-p symbol-name)
          (ruby-userdef-p symbol-name))
      (intern symbol-name package)
      (progn
        (unless (valid-ruby-symbol-p symbol-name)
          (error "Unsupported Marshal data. Symbol must not have hyphens (#\-). If it is a class name, declare it.~%~A"
                 symbol-name))
        (intern (substitute #\- #\_ (string-upcase symbol-name)) package))))

(defun lisp-symbol->ruby-symbol (symbol)
  (unless (valid-lisp-symbol-p symbol)
    (error "Unsupported symbol format. Symbol must be upper case and without #\_~%~A"
           symbol))
  (substitute #\_ #\- (string-downcase (symbol-name symbol))))
