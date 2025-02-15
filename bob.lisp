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

;; Code scaffolding to make loading/building the system easy.  Named after "Bob
;; the Builder".

;;; Code:

;; Try and push the current directory into the quicklisp project directories.
(pushnew (truename (uiop:getcwd))
         ql:*local-project-directories*)

(defpackage bob
  (:use :cl)
  (:export :build :qload))

(in-package :bob)

(defun qload ()
  (ql:quickload :cantedraw))

(defun build ()
  (qload)
  (asdf:make :cantedraw))
