(in-package :marshal)

(defconstant +nil+ #.(char-code #\0))
(defconstant +true+ #.(char-code #\T))
(defconstant +false+ #.(char-code #\F))
(defconstant +integer+ #.(char-code #\i))
(defconstant +array+ #.(char-code #\[))
(defconstant +hash+ #.(char-code #\{))
(defconstant +hash-default+ #.(char-code #\}))
(defconstant +symbol+ #.(char-code #\:))
(defconstant +symlink+ #.(char-code #\;))
(defconstant +object-ref+ #.(char-code #\@))
(defconstant +ivar+ #.(char-code #\I))
(defconstant +ascii+ #.(char-code #\"))
(defconstant +object+ #.(char-code #\o))
(defconstant +userdef+ #.(char-code #\u))

(defun make-ruby-lisp-pair (ruby lisp) (cons ruby lisp))
(defun get-ruby (pair) (car pair))
(defun get-lisp (pair) (cdr pair))

(defvar class-list nil)
(defvar userdef-list nil)

(defun add-class (ruby-name lisp-name)
  (pushnew (make-ruby-lisp-pair ruby-name lisp-name) class-list :test #'equal))

(defun add-userdef (ruby-name lisp-name)
  (pushnew (make-ruby-lisp-pair ruby-name lisp-name) userdef-list :test #'equal))

(defgeneric userdef-decode (object len bytes))
(defgeneric userdef-encode (object))

(defun find-lisp-userdef (symbol)
  (declare (type string symbol))
  (or (get-lisp (find symbol userdef-list :key #'get-ruby :test #'string=))
      (error "Userdef ~A is not declared.~%Eval (~A \"~A\" 'SOME-CLASS-NAME)~%Then ~A and ~A must be implemented for that class."
             symbol 'add-userdef symbol 'userdef-decode 'userdef-encode)))

(defun find-lisp-class (symbol)
  (declare (type string symbol))
  (or (get-lisp (find symbol class-list :key #'get-ruby :test #'string=))
      (error "Class ~A is not declared.~%Eval (~A \"~A\" 'SOME-CLASS-NAME)."
             symbol 'add-class symbol)))

(defun ruby-class-p (ruby-name)
  (member ruby-name class-list :key #'get-ruby :test #'string=))
(defun ruby-userdef-p (ruby-name)
  (member ruby-name userdef-list :key #'get-ruby :test #'string=))

(defun lisp-class-p (lisp-name)
  (member lisp-name class-list :key #'get-lisp))
(defun lisp-userdef-p (lisp-name)
  (member lisp-name userdef-list :key #'get-lisp))

(defun find-ruby-class (lisp-name)
  (get-ruby (find lisp-name class-list :key #'get-lisp)))
(defun find-ruby-userdef (lisp-name)
  (get-ruby (find lisp-name userdef-list :key #'get-lisp)))
