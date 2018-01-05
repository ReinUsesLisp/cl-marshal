(in-package :marshal)

(defvar class-list (make-hash-table :test 'equal))
(defvar userdef-list (make-hash-table :test 'equal))

(defun add-class (ruby-name lisp-name)
  (setf (gethash ruby-name class-list) lisp-name))

(defun add-userdef (ruby-name lisp-name)
  (setf (gethash ruby-name userdef-list) lisp-name))

(defgeneric userdef-decode (object len bytes))
(defgeneric userdef-encode (object))

(defun find-lisp-userdef (symbol)
  (or (gethash (symbol-name symbol) userdef-list)
      (error "Userdef \"~A\" is not declared.~%Eval (~A \"~A\" 'SOME-CLASS-NAME)~%Then ~A and ~A must be implemented for that class." symbol 'add-userdef symbol 'userdef-decode 'userdef-encode)))

(defun find-lisp-class (symbol)
  (or (gethash (symbol-name symbol) class-list)
      (error "Class \"~A\" is not declared.~%Eval (~A \"~A\" 'SOME-CLASS-NAME)."
             symbol 'add-class symbol)))
