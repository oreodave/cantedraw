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
