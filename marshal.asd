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

(asdf:defsystem #:marshal
  :description "Ruby's Marshal library practical implementation."
  :author "ReinUsesLisp <reinuseslisp@airmail.cc>"
  :license "LLGPL"
  :depends-on (:alexandria :trivial-utf-8 :anaphora :closer-mop :flexi-streams)
  :pathname "src"
  :components ((:file "package")
               (:file "helpers" :depends-on ("package"))
               (:file "marshal" :depends-on ("package"))
               (:file "decode" :depends-on ("marshal" "helpers"))
               (:file "encode" :depends-on ("marshal" "helpers"))))
