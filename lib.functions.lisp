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

(fn parse-integer* (inp) (-> (string) (or integer list))
  (parse-integer inp :junk-allowed t))

(fn take (n lst) (-> (fixnum list) list)
  (subseq lst 0 n))
