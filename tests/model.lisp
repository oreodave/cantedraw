;;; model.lisp - 2025-02-21

;; Copyright (C) 2025 Aryadev Chavali

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License Version 2 for
;; details.

;; You may distribute and modify this code under the terms of the GNU General
;; Public License Version 2, which you should have received a copy of along with
;; this program.  If not, please go to <https://www.gnu.org/licenses/>.

;;; Code:

(defpackage cantedraw/tests/model
  (:use
   :cl :parachute
   :cantedraw.lib.macros :cantedraw.lib.functions
   :cantedraw/tests/functions
   :cantedraw.model))

(in-package :cantedraw/tests/model)

(define-test model-test
  :depends-on ((cantedraw/tests/macros macro-test)
               (cantedraw/tests/functions function-test)))
