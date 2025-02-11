;;; bob.lisp - 2025-02-10

;; Copyright (C) 2025 Aryadev Chavali

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License Version 2 for
;; details.

;; You may distribute and modify this code under the terms of the GNU General
;; Public License Version 2, which you should have received a copy of along with
;; this program.  If not, please go to <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Little bit of code scaffolding that makes it easy to load or build the
;; system.  Named after "Bob the Builder".

;;; Code:

(defpackage bob
  (:use :cl)
  (:export
   :build :load-all))

(in-package :bob)

(ql:quickload :deploy)
(asdf:load-asd (merge-pathnames "cantedraw.asd" (uiop:getcwd)))

(defun build ()
  (push :deploy-console *features*)
  (asdf:make :cantedraw))

(defun load-all ()
  (asdf:load-system :cantedraw))
