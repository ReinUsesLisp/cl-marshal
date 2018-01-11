;;;;
;;;;    This file is part of cl-marshal.
;;;;
;;;;    cl-marshal is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Lesser General Public License as
;;;;    published by the Free Software Foundation, either version 3 of the
;;;;    License, or (at your option) any later version.
;;;;
;;;;    cl-marshal is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Lesser General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Lesser General Public License
;;;;    along with cl-marshal.  If not, see <http://www.gnu.org/licenses/>.
;;;;

(in-package :marshal)

(defun decode (bytes &optional (package *package*))
  (decode-stream (make-in-memory-input-stream bytes) package))

(defun decode-file (path &optional (package *package*))
  (with-open-file (file path :element-type '(unsigned-byte 8))
    (decode-stream file package)))

(defun decode-stream (file package)
  (declare (type stream file))
  (let ((major (read-byte file))
        (minor (read-byte file)))
    (when (or (/= major 4) (/= minor 8))
      (error "Marshal version ~A.~A is invalid." major minor))
    (%decode file
             (make-hash-table :test #'equal)
             (make-hash-table :test #'equal)
             package)))

(defun %decode (file syms objs package)
  (eswitch ((read-byte file))
    (+nil+ nil)
    (+true+ 'true)
    (+false+ 'false)
    (+integer+ (decode-integer file))
    (+array+ (decode-array file syms objs package))
    (+hash+ (decode-hash file syms objs package))
    (+symbol+ (decode-symbol file syms package))
    (+symlink+ (decode-symlink file syms))
    (+object-ref+ (decode-object-ref file objs))
    (+ivar+ (decode-ivar file syms objs package))
    (+ascii+ (decode-ascii file objs))
    (+object+ (decode-object file syms objs package))
    (+userdef+ (decode-userdef file syms objs package))))

(defun decode-userdef (file syms objs package)
  (let ((class (%decode file syms objs package))
        (length (decode-integer file)))
    (unless (symbolp class)
      (error "Invalid Marshal data (class name is not a symbol)."))
    (let ((object (make-instance (find-lisp-userdef (symbol-name class))))
          (previous-position (file-position file)))
      (userdef-decode object file length)
      (file-position file (+ previous-position length)))))

(defun decode-object (file syms objs package)
  (push-to-cache objs
    (let ((class (%decode file syms objs package)))
      (unless (symbolp class)
        (error "Invalid Marshal data (class name is not a symbol)."))
      (destructuring-bind (raw-keys values) (read-pairs file syms objs package)
        (let* ((lisp-class (find-lisp-class (symbol-name class)))
               (object (make-instance lisp-class))
               (keys (mapcar #'symbol->slot raw-keys)))
          (awhen (remove-if (lambda (key) (slot-exists-p object key)) keys)
            (error "Class ~A is lacking ~A." lisp-class it))
          (loop for key in keys
             for value in values
             do (setf (slot-value object key) value))
          object)))))

(defun decode-ascii (file objs)
  ;; I think it's not a good idea to solve invalid inputs returning them as a
  ;; byte sequence because "invalid" strings (e.g.: zlib data) may randomly be
  ;; stored as a "valid" string.
  (push-to-cache objs
    (let ((bytes (read-bytes file)))
      (handler-case (utf-8-bytes-to-string bytes)
        ((or utf-8-decoding-error) ()
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

(defun decode-symlink (file syms)
  (or (clone (gethash (decode-integer file) syms))
      (error "Invalid Marshal data (invalid symlink).")))

(defun decode-symbol (file syms package)
  (push-to-cache syms
    (ruby-symbol->lisp-symbol
     (read-utf-8-string file :byte-length (decode-integer file))
     package)))

(defun decode-array (file syms objs package)
  (push-to-cache objs
    (loop for i from 0 upto (1- (decode-integer file))
       collect (%decode file syms objs package))))

(defun decode-hash (file syms objs package)
  (push-to-cache objs
    (let ((hash (make-hash-table :test #'equal)))
      (loop with (keys values) = (read-pairs file syms objs package)
         for key in keys
         for value in values
         do (setf (gethash key hash) value))
      hash)))

(defun decode-ivar (file syms objs package)
  (push-to-cache objs
    (let ((mode (read-byte-char file))
          (bytes (read-bytes file)))
      ;; for now, ignore key values
      (read-pairs file syms objs package)
      (ecase mode
        (#\" (utf-8-bytes-to-string bytes))
        (#\/ (error "Regex are not implemented (yet®)."))))))

(defun read-bytes (file)
  (let* ((len (decode-integer file))
         (bytes (make-array len :element-type '(unsigned-byte 8))))
    (dotimes (i len)
      (setf (aref bytes i) (read-byte file)))
    bytes))

(defun read-pairs (file syms objs package)
  (loop with keys = nil
     with values = nil
     for i from 0 upto (1- (decode-integer file))
     do (push (%decode file syms objs package) keys)
     do (push (%decode file syms objs package) values)
     finally (return-from read-pairs
               (list (nreverse keys) (nreverse values)))))
