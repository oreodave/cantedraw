;;; lib.functions.lisp - 2025-02-09

;; Copyright (C) 2025 Aryadev Chavali

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License Version 2 for
;; details.

;; You may distribute and modify this code under the terms of the GNU General
;; Public License Version 2, which you should have received a copy of along with
;; this program.  If not, please go to <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Helpful functions for usage throughout the project.

;;; Code:

(in-package :lib.functions)

(fn range (start end &optional (step 1)) (-> (fixnum fixnum &optional fixnum) list)
  "Make a list of numbers from START to END (exclusive).  If STEP is given, then
each member is STEP distance apart."
  (if (<= end start)
      (error (format nil "~a < ~a" end start))
      (loop :for i :from start :to (1- end) :by step
            :collect i)))

(fn take (n lst) (-> (fixnum list) list)
  "Return the first N elements of LST."
  (subseq lst 0 n))

(fn split (n lst) (-> (fixnum list) list)
  "Return CONS where CAR is the first N elements of LST and CDR is the rest."
  (cons (take n lst)
        (subseq lst n)))

(fn rev-map (indicator lst &key (key-eq #'eq))
    (-> (function list &key (:key-eq function)) list)
  "Given LST and INDICATOR: LST -> A, return an association list A -> 2^LST
where key x in A has associations {y in LST : INDICATOR(y) = x}."
  (loop :with assoc-list := nil
        :for element :in lst
        :for key = (funcall indicator element)
        :if (assoc key assoc-list :test key-eq)
          :do (->> (alist-val key assoc-list)
                   (cons element)
                   (setf (alist-val key assoc-list)))
        :else
          :do (setq assoc-list (cons (list key element) assoc-list))
        :finally (return assoc-list)))

(fn parse-integer* (inp) (-> (string) (or integer list))
  "Given string INP, attempt to parse an integer.  Return NIL otherwise."
  (parse-integer inp :junk-allowed t))
