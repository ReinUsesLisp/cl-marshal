;;;;
;;;;    This file is part of cl-rmarshal.
;;;;
;;;;    cl-rmarshal is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Lesser General Public License as
;;;;    published by the Free Software Foundation, either version 3 of the
;;;;    License, or (at your option) any later version.
;;;;
;;;;    cl-rmarshal is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Lesser General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Lesser General Public License
;;;;    along with cl-rmarshal.  If not, see <http://www.gnu.org/licenses/>.
;;;;

(in-package :rmarshal)

(defun encode (object)
  (let ((file (make-in-memory-output-stream)))
    (encode-stream object file)
    (get-output-stream-sequence file)))

(defun encode-file (object path)
  (with-open-file (file path
                        :element-type '(unsigned-byte 8)
                        :direction :output
                        :if-exists :supersede)
    (encode-stream object file)))

(defun encode-stream (object file)
  (declare (type stream file))
  ;; write version
  (write-byte 4 file)
  (write-byte 8 file)
  (%encode object file))

(defun %encode (object file)
  (funcall
   (typecase object
     (null #'encode-nil)
     (integer #'encode-integer)
     (symbol (case object
               ((true false) #'encode-boolean)
               (otherwise #'encode-symbol)))
     (cons #'encode-array)
     (hash-table #'encode-hash-table)
     (standard-object (cond
                        ((lisp-class-p (type-of object)) #'encode-object)
                        ((lisp-userdef-p (type-of object)) #'encode-userdef)))
     (string #'encode-string)
     ((simple-array (unsigned-byte 8) (*)) #'encode-bytes)
     (otherwise (error "Object can not be encoded in a Ruby Marshal format (or it's not implemented yet).~%~A" object)))
   object file))

(defun encode-userdef (object file)
  (declare (type standard-object object))
  (write-byte +userdef+ file)
  (encode-symbol (find-ruby-userdef (type-of object)) file)
  (let ((temporal-buffer (make-in-memory-output-stream)))
    (userdef-encode object temporal-buffer)
    (let ((bytes (get-output-stream-sequence temporal-buffer)))
      (write-integer (length bytes) file)
      (write-sequence bytes file))))

(defun encode-object (object file)
  (declare (type standard-object object))
  (write-byte +object+ file)
  (encode-symbol (find-ruby-class (type-of object)) file)
  (let ((slot-definitions (closer-mop:class-direct-slots (class-of object))))
    (write-integer (length slot-definitions) file)
    (loop for slot in (mapcar #'closer-mop:slot-definition-name slot-definitions)
       do (progn (encode-symbol (format nil "@~A" (lisp-symbol->ruby-symbol slot)) file)
                 (%encode (slot-value object slot) file)))))

(defun encode-bytes (object file)
  (declare (type (simple-array (unsigned-byte 8) (*)) object))
  (write-byte +ascii+ file)
  (write-integer (array-dimension object 0) file)
  (loop for byte across object
     do (write-byte byte file)))

(defun encode-string (object file)
  (declare (type string object))
  (if (every #'standard-char-p object)
      ;; every single character is ascii
      (progn
        (write-byte +ascii+ file)
        (write-integer (length object) file)
        (loop for byte in (map 'list #'char-code object)
           do (write-byte byte file)))
      ;; utf-8, use ivar
      (progn
        (write-byte +ivar+ file)
        (write-byte #.(char-code #\") file)
        (write-integer (utf-8-byte-length object) file)
        (write-utf-8-bytes object file :null-terminate nil)
        (write-integer 1 file)
        (write-byte #.(char-code #\:) file)
        (write-integer 1 file)
        (write-byte #.(char-code #\E) file)
        (write-byte #.(char-code #\T) file))))

(defun encode-array (object file)
  (declare (type list object))
  (write-byte +array+ file)
  (write-integer (length object) file)
  (loop for element in object
     do (%encode element file)))

(defun encode-hash-table (object file)
  (declare (type hash-table object))
  (write-byte +hash+ file)
  (write-integer (hash-table-count object) file)
  (loop for key being the hash-keys of object
     for value being the hash-values of object
     do (progn (%encode key file)
               (%encode value file))))

(defun encode-symbol (object file)
  (declare (type (or symbol string) object))
  (write-byte +symbol+ file)
  (let ((name (if (symbolp object)
                  (lisp-symbol->ruby-symbol object)
                  object)))
    (write-integer (utf-8-byte-length name) file)
    (write-utf-8-bytes name file :null-terminate nil)))

(defun encode-nil (object file)
  (declare (ignore object) (type null object))
  (write-byte +nil+ file))

(defun encode-boolean (object file)
  (declare (type symbol object))
  (write-byte (ecase object
                (true +true+)
                (false +false+))
              file))

(defun integer-bytes (integer)
  (let ((positive (if (minusp integer) (- integer) integer)))
    (cond ((< positive #xFF) 1)
          ((< positive #xFFFF) 2)
          ((< positive #xFFFFFF) 3)
          ((< positive #xFFFFFFFF) 4)
          (t (error "Bignum encoding support is not implemented.~%~A" integer)))))

(defun encode-integer (object file)
  (declare (type integer object))
  (write-byte +integer+ file)
  (write-integer object file))

(defun write-integer (integer file)
  (cond
    ((zerop integer) (write-byte 0 file))
    ((and (> integer 0) (<= integer 122)) (write-byte (+ integer 5) file))
    ((> integer 122)
     (let ((bytes (integer-bytes integer)))
       (write-byte bytes file)
       (write-integer-bytes bytes integer file)))
    ((>= integer -123) (write-byte (+ integer #xFB) file))
    ((< integer -123)
     (let ((bytes (integer-bytes integer)))
       (write-byte (- #x100 bytes) file)
       (write-integer-bytes bytes (+ integer (bytes->power2 bytes)) file)))))

(defun write-integer-bytes (bytes integer file)
  ;; TODO test in big endian
  (dotimes (i bytes)
    (write-byte (logand #xFF (ash integer (- (bytes->bits i)))) file)))
