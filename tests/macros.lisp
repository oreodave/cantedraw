;;; main.lisp - 2025-02-16

;; Copyright (C) 2025 Aryadev Chavali

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License Version 2 for
;; details.

;; You may distribute and modify this code under the terms of the GNU General
;; Public License Version 2, which you should have received a copy of along with
;; this program.  If not, please go to <https://www.gnu.org/licenses/>.

;;; Code:

(defpackage cantedraw/tests/macros
  (:use :cl :cantedraw.lib.macros
        :parachute))

(in-package :cantedraw/tests/macros)

(define-test macro-test)

(define-test (macro-test "-->")
  (true (null          (--> x)))
  (is eq 'a            (--> x 'a))
  (is eq 2             (--> x 1 (1+ x)))
  (is eq 2             (--> x 1 1+))
  (is string= "World!" (--> _ "Hello" (format nil "~a World!" _) (subseq _ 6))))

(define-test (macro-test "->>")
  (true (null (->>)))
  (let ((a (gensym))
        (b (gensym))
        (c (gensym))
        (d (gensym)))
    (is eq a (->> a))
    (is equal `(,a ,b)           (macroexpand `(->> ,b (,a))))
    (is equal `(,a ,b)           (macroexpand `(->> ,b ,a)))
    (is equal `(,a (,b ,c))      (macroexpand `(->> ,c (,b) (,a))))
    (is equal `(,a (,b ,c))      (macroexpand `(->> ,c ,b ,a)))
    (is equal `(,a ,b ,c)        (macroexpand `(->> ,c (,a ,b))))
    (is equal `(,a ,b (,c ,d))   (macroexpand `(->> (,c ,d) (,a ,b))))
    (is equal `(,a (,b (,c ,d))) (macroexpand `(->> ,d (,c) (,b) (,a))))
    (is equal `(,a (,b (,c ,d))) (macroexpand `(->> ,d ,c ,b ,a))))
  (is string= "Hello, World!"    (->> "world!" (format nil "hello, ~a") string-capitalize)))

(define-test (macro-test "-<>")
  (true (null (-<>)))
  (let ((a (gensym))
        (b (gensym))
        (c (gensym))
        (d (gensym)))
    (is eq a (-<> a))
    (is equal `(,a ,b)           (macroexpand `(-<> ,b (,a))))
    (is equal `(,a ,b)           (macroexpand `(-<> ,b ,a)))
    (is equal `(,a (,b ,c))      (macroexpand `(-<> ,c (,b) (,a))))
    (is equal `(,a (,b ,c))      (macroexpand `(-<> ,c ,b ,a)))
    (is equal `(,a ,b ,c)        (macroexpand `(-<> ,b (,a ,c))))
    (is equal `(,a (,b (,c ,d))) (macroexpand `(-<> ,d (,c) (,b) (,a))))
    (is equal `(,a (,b (,c ,d))) (macroexpand `(-<> ,d ,c ,b ,a)))
    (is equal `(,a (,b ,c) ,d)   (macroexpand `(-<> ,c (,b) (,a ,d)))))
  (is equal 69                   (-<> 489 (- 420)))
  (is string= "HELLO"            (-<> "Hello World!" (subseq 0 5) string-upcase)))
