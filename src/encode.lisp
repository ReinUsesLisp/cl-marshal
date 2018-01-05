(in-package :marshal)

(defun encode-file (path object)
  (with-open-file (file path
                        :element-type '(unsigned-byte 8)
                        :direction :output
                        :if-exists :supersede)
    ;; write version
    (write-byte 4 file)
    (write-byte 8 file)
    (encode object file)))

(defun encode (object file)
  (declare (type stream file))
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
                       ((class-p (type-of object)) #'encode-object)
                       ((userdef-p (type-of object)) #'encode-userdef)))
    (string #'encode-string)
    (otherwise (error "Object can not be encoded in a Ruby Marshal format (or it's not implemented yet).~%~A~%" object)))
   object file))

(defun encode-nil (object file)
  (declare (ignore object))
