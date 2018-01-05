(in-package :marshal)

(defun slot-char-p (char)
  (if (alpha-char-p char)
      (lower-case-p char)
      t))

(defun symbol->slot (symbol)
  (let ((name (symbol-name symbol)))
    (when (or (char/= (char name 0) #\@) (not (every #'slot-char-p name)))
      (error "Invalid Marshal data (unhandled slot name ~A)." symbol))
    (intern (substitute #\- #\_ (string-upcase (subseq name 1))))))

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
