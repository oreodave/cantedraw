;;; main.lisp - 2025-02-11

;; Copyright (C) 2025 Aryadev Chavali

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License Version 2 for
;; details.

;; You may distribute and modify this code under the terms of the GNU General
;; Public License Version 2, which you should have received a copy of along with
;; this program.  If not, please go to <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Defines the entrypoint of the program, handling any input from the user and
;; passing it into the system.

;;; Code:

(in-package :cantedraw.main)

(fn read-input (&optional (prompt "> ")) (-> (&optional string) string)
  (format t "~a" prompt)
  (force-output)
  (read-line))

(fn parse-integers (input) (-> (string) list)
  (->> input
       uiop:split-string
       (mapcar #'parse-integer*)
       (remove-if #'null)))

(fn read-integers () (-> nil list)
  (->> (read-input "Enter numbers: ")
       parse-integers))

(fn read-until-integers () (-> nil list)
  (let ((inp (read-integers)))
    (while (null inp)
      (format t "Need at least one integer...~%")
      (force-output)
      (setq inp (read-integers)))
    inp))

(defun start ()
  (--> (read-until-integers)
       (cons '+ _)
       (format t "~a = ~a~%" _ (eval _))))
