;;; lib.macros.lisp - 2025-02-09

;; Copyright (C) 2025 Aryadev Chavali

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License Version 2 for
;; details.

;; You may distribute and modify this code under the terms of the GNU General
;; Public License Version 2, which you should have received a copy of along with
;; this program.  If not, please go to <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Helpful macros for usage throughout the project.

;;; Code:

(in-package :cantedraw.lib.macros)

(defmacro --> (placeholder &body forms)
  "Lexically bind current form as `placeholder' for use in the next form, returning
the result of the last form.

i.e.

(--> (a1 a2...) (b1 b2...) (c1 c2...)) =
(let* ((placeholder (a1 a2 ...))
       (placeholder (b1 b2 ...))
       (placeholder (c1 c2 ...)))
    _ )

Also includes transformer where symbols are considered unary functions i.e.
(--> x y) <-> (--> x (y placeholder)).
"
  (if (null forms)
      nil
      (let ((assignment-forms
              (loop :for f :in forms
                    :for canon-f := (if (symbolp f)
                                        (list f placeholder)
                                        f)
                    :collect `(,placeholder ,canon-f))))
        `(let* ,assignment-forms
           ,placeholder))))

(defmacro ->> (&rest forms)
  "Make current form the last argument of the next form, returning the last
  form.

i.e.
(->> (a1 a2...) (b1 b2...) (c1 c2...)) == (c1 c2 ... (b1 b2 ... (a1 a2 ...)))

Also includes transformer where symbols are considered unary functions.

Like the `|>' operator in Ocaml."
  (if (null forms)
      nil
      (loop :with acc = (car forms)
            :for func :in (cdr forms)
            :for canon-func = (if (symbolp func) (list func) func)
            :do (setq acc (append canon-func (list acc)))
            :finally (return acc))))

(defmacro while (condition &body body)
  `(loop :while ,condition
         :do
         (progn ,@body)))

(deftype -> (args result)
  "Type alias for function."
  `(function ,args ,result))

(defmacro fn (name lambda-list type &body body)
  "Construct a function `NAME' with a declared function type `TYPE' that takes
arguments `LAMBDA-LIST' with body `BODY'."
  `(progn
     (declaim (ftype ,type ,name))
     (defun ,name ,lambda-list
       ,@body)))

(defmacro $ (capture &rest forms)
  "Given a sequence of FORMS, return a unary function which applies each form
sequentially"
  `(lambda (,capture)
     (->> ,capture ,@forms)))

(defmacro alist-val (key alist)
  "Helper macro for getting the value of KEY in ALIST."
  `(cdr (assoc ,key ,alist)))
