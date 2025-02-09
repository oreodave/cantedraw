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

(defun read-integers ()
  (->> (read-input "Enter numbers: ")
       (uiop:split-string)
       (mapcar (lambda (s) (parse-integer s :junk-allowed t)))
       (remove-if #'null)))

(defun read-until-integers ()
  (let ((inp (read-integers)))
    (while (null inp)
      (format t "Need at least one integer...~%")
      (setq inp (read-integers)))
    inp))

(defun start ()
  (--> (read-until-integers)
       (format t "~a = ~a~%" (cons '+ _) (reduce #'+ _))))
