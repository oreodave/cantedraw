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
  (->> "Enter numbers: "
       read-input
       parse-integers))

(fn read-until-integers () (-> nil list)
  (let ((inp (read-integers)))
    (while (null inp)
      (format t "Need at least one integer...~%")
      (force-output)
      (setq inp (read-integers)))
    inp))

(fn is-valid-hand-index (n) (-> (fixnum) boolean)
  (and (< n 5)
       (>= n 0)))

(fn read-until-valid-integers () (-> nil list)
  (let ((inp (remove-duplicates (read-integers))))
    (while (not (and (every #'is-valid-hand-index inp)
                     (< (length inp) 5)
                     (>= (length inp) 0)))
      (format t "Need at most 5 integers between 0 and 4...~%")
      (force-output)
      (setq inp (remove-duplicates (read-integers))))
    inp))

(fn read-and-confirm-valid-integers (hand) (-> nil list)
  (let ((confirm nil)
        inp)
    (while (null confirm)
      (setq inp (read-until-valid-integers))
      (if (null inp)
          (format t "No redeal (end of game)~%")
          (->> (loop :for index :in inp
                     :collect (nth index hand))
               cardset->str
               (format t "To redeal: ~a~%")))
      (setq confirm (y-or-n-p "Confirm: ")))
    inp))

(defun print-hand (hand)
  (->> hand cardset->str (format t "Hand=[~a]~%")))

(defun read-redeal-print (hand deck)
  (cond
    ((<= (length deck) 5)
     (cons hand deck))
    ((null hand)
     (destructuring-bind ((hand) . deck) (deal-hands 1 deck)
       (print-hand hand)
       (read-redeal-print hand deck)))
    (t
     (let ((indices (read-and-confirm-valid-integers hand)))
       (if (null indices)
           (cons hand deck)
           (destructuring-bind (hand . deck) (redeal-hand hand indices deck)
             (print-hand hand)
             (read-redeal-print hand deck)))))))

(defun start ()
  (setf *random-state* (make-random-state t))
  (destructuring-bind (final-hand . rest)
      (->> (make-deck)
           alexandria:shuffle
           (read-redeal-print nil))
    (format t "Cards remaining: {~a}

Final hand: [~a]"
            (cardset->str rest)
            (cardset->str final-hand))))
