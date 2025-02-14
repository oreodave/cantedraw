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

(defpackage cantedraw.lib.macros
  (:use :cl)
  (:export
   :--> :->>
   :-> :fn
   :while :alist-val
   :$))

(defpackage cantedraw.lib.functions
  (:use :cl :cantedraw.lib.macros)
  (:export
   :parse-integer*
   :range :take :split
   :rev-map))

(defpackage cantedraw.main
  (:use :cl :cantedraw.lib.macros :cantedraw.lib.functions)
  (:export :start))
