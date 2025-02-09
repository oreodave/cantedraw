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

(in-package :lib.macros)

(defmacro --> (&rest functions)
  "Lexically bind current form as `_' for use in the next form, returning the
  result of the last form.

i.e.

(--> (a1 a2...) (b1 b2...) (c1 c2...)) =
(let* ((_ (a1 a2 ...))
       (_ (b1 b2 ...))
       (_ (c1 c2 ...)))
    _ )

Also includes transformer where symbols are considered unary functions i.e.
(--> x y) <-> (--> x (y _)).
"
  (if (null functions)
      nil
      (let ((assignment-forms
              (loop :for f :in functions
                    :for canon-f = (if (symbolp f) (list f 'lib.macros:_) f)
                    :collect `(lib.macros:_ ,canon-f))))
        `(let* ,assignment-forms
           lib.macros:_))))

(defmacro ->> (&rest functions)
  "Make current form the last argument of the next form, returning the last
  form.

i.e.
(->> (a1 a2...) (b1 b2...) (c1 c2...)) == (c1 c2 ... (b1 b2 ... (a1 a2 ...)))

Also includes transformer where fun in functions that are symbols are considered
unary functions.

Like the `|>' operator in Ocaml."
  (if (null functions)
      nil
      (loop :with acc = (car functions)
            :for func :in (cdr functions)
            :for canon-func = (if (symbolp func) (list func) func)
            :do (setq acc (append canon-func (list acc)))
            :finally (return acc))))

(defmacro while (condition &body body)
  `(loop :while ,condition
         :do
         (progn ,@body)))
