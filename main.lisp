;;; main.lisp - 2025-02-09

;; Copyright (C) 2025 Aryadev Chavali

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License Version 2 for
;; details.

;; You may distribute and modify this code under the terms of the GNU General
;; Public License Version 2, which you should have received a copy of along with
;; this program.  If not, please go to <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Implementation of `main' package.  Defines the entrypoint of the program,
;; handling any input from the user and passing it into the system.

;;; Code:

(in-package :main)

(defun read-input (&optional (prompt "> "))
  (format t "~a" prompt)
  (force-output)
  (read-line))

(defun start ()
  (->> (read-input "Enter name: ")
       (format t "Hello, ~a!~%")))
