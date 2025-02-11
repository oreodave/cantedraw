;;; packages.lisp - 2025-02-09

;; Copyright (C) 2025 Aryadev Chavali

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License Version 2 for
;; details.

;; You may distribute and modify this code under the terms of the GNU General
;; Public License Version 2, which you should have received a copy of along with
;; this program.  If not, please go to <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Define the packages (namespaces) involved in this project.

;;; Code:

(defpackage lib.macros
  (:use :cl)
  (:export
   :_ :--> :->>
   :-> :fn
   :while :alist-val
   :$))

(defpackage lib.functions
  (:use :cl :lib.macros)
  (:export
   :parse-integer*
   :range :take :split
   :rev-map))

(defpackage odraw
  (:use :cl :lib.macros :lib.functions)
  (:export :start))
