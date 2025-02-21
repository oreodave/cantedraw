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

(define-test function-test
  :depends-on ((cantedraw/tests/macros macro-test)))

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
  :compile-at :execute
  (fail (take nil nil))
  (fail (take 100 nil))
  (fail (take nil 100))
  (true (->> (list 1 2 3 4) (take 0) null))
  (is equal "H" (take 1 "Hello"))
  (is equal '(1 2) (take 2 '(1 2 3 4 5))))
