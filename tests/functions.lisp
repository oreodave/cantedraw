;;; functions.lisp - 2025-02-20

;; Copyright (C) 2025 Aryadev Chavali

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License Version 2 for
;; details.

;; You may distribute and modify this code under the terms of the GNU General
;; Public License Version 2, which you should have received a copy of along with
;; this program.  If not, please go to <https://www.gnu.org/licenses/>.

;;; Code:

(defpackage cantedraw/tests/functions
  (:use
   :cl :parachute :cantedraw.lib.macros
   :cantedraw.lib.functions))

(in-package :cantedraw/tests/functions)

(define-test function-test)

(define-test (function-test "parse-integer*")
  :compile-at :execute
  (is eq 2    (parse-integer* "2"))
  (is eq 2048 (parse-integer* "2048abcdef"))
  (is eq nil    (parse-integer* "a2048abcdef"))
  (is eq nil    (parse-integer* "garbage"))
  (fail       (parse-integer* nil)))

(define-test (function-test range)
  :compile-at :execute
  (fail (range 1 0))
  (fail (range nil nil))
  (fail (range "a" "b"))
  (true (null (range 1 1)))
  (is equal '(1 2 3 4)    (range 1 5))
  (is equal '(-3 -2 -1 0) (range -3 1)))

(define-test (function-test take)
  :depends-on ((cantedraw/tests/macros ->>))
  :compile-at :execute
  (fail (take nil nil))
  (fail (take 100 nil))
  (fail (take nil 100))
  (true (->> (list 1 2 3 4) (take 0) null))
  (is equal "H" (take 1 "Hello"))
  (is equal '(1 2) (take 2 '(1 2 3 4 5))))

(define-test (function-test split)
  :compile-at :execute
  (fail (split nil nil))
  (fail (split 100 nil))
  (fail (split nil 100))
  (is-values (split 0 '(1 2 3 4))
    (eq nil)
    (equal '(1 2 3 4)))
  (is-values (split 1 '(1 2 3 4))
    (equal '(1))
    (equal '(2 3 4)))
  (is-values (split 5 "Hello World")
    (string= "Hello")
    (string= " World")))

(define-test (function-test rev-map)
  :depends-on (range
               (cantedraw/tests/macros ->>))
  :compile-at :execute
  (fail (rev-map nil nil))
  (fail (rev-map "a string" "another string" :key-eq "not a function"))
  (true (->> nil (rev-map #'identity) null))
  (let ((res (rev-map #'evenp (range 1 7))))
    (false (null res))
    (is equal 2 (length res))
    (is equal 3 (->> (assoc t res) cdr length))
    (is equal 3 (->> (assoc nil res) cdr length))
    (true (->> (assoc t res) cdr (every #'evenp)))
    (true (->> (assoc nil res) cdr (every #'oddp))))
  (let* ((mod* (lambda (n) (mod n 3)))
         (res (rev-map mod* (range 1 12))))
    (false (null res))
    (is equal 3 (length res))
    (is equal 3 (->> (assoc 0 res) cdr length))
    (is equal 4 (->> (assoc 1 res) cdr length))
    (is equal 4 (->> (assoc 2 res) cdr length))
    (true (->> (assoc 0 res) cdr (every (lambda (x) (= (mod x 3) 0)))))
    (true (->> (assoc 1 res) cdr (every (lambda (x) (= (mod x 3) 1)))))
    (true (->> (assoc 2 res) cdr (every (lambda (x) (= (mod x 3) 2))))))
  (let ((res (rev-map #'identity "lots of letters")))
    (false (null res))
    (is equal 2 (->> (assoc #\l res) cdr length))
    (is equal 3 (->> (assoc #\t res) cdr length))
    (is equal 2 (->> (assoc #\space res) cdr length))
    (is equal 2 (->> (assoc #\s res) cdr length))))

(define-test (function-test remove-at-indices)
  :depends-on (range)
  :compile-at :execute
  (fail (remove-at-indices "a string" "another string"))
  (true (null (remove-at-indices nil nil)))
  (is equal '(1 2 3) (remove-at-indices nil '(1 2 3)))
  (is equal '(2) (remove-at-indices '(0 2) '(1 2 3)))
  (let* ((inp (range 100 200))
         (t1 (remove-at-indices (range 0 100 2) inp))
         (t2 (remove-at-indices (range 1 100 2) inp)))
    (is equal 50 (length t1))
    (is equal 50 (length t2))
    (true (every (lambda (n) (not (member n t2))) t1))
    (true (every (lambda (n) (not (member n t1))) t2)))
  (let* ((indices (list 0 5 6 7 8 9 10))
         (str-lst (remove-at-indices indices "Hello World"))
         (ret (coerce str-lst 'string)))
    (is string= "ello" ret)))
