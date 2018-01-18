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

(defpackage :rmarshal
  (:use #:cl #:alexandria #:trivial-utf-8 #:anaphora #:flexi-streams)
  (:export #:true #:false
           #:add-class #:add-userdef
           #:userdef-decode #:userdef-encode
           #:decode #:decode-file #:decode-stream
           #:encode #:encode-file #:encode-stream))
