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
   :cantedraw.model))

(in-package :cantedraw/tests/model)

(define-test model-test)

(define-test (model-test int->suit)
  :depends-on ((cantedraw/tests/macros ->>)
               (cantedraw/tests/functions rev-map))
  :compile-at :execute
  (fail (int->suit nil))
  (fail (int->suit "Not a number"))
  ;; Proving int->suit splits 0-51 perfectly between the 4 suits
  (let ((mapping (rev-map #'int->suit (range 0 53))))
    (is eq 5 (length mapping))
    (let ((spades   (alist-val :spades mapping))
          (hearts   (alist-val :hearts mapping))
          (clubs    (alist-val :clubs mapping))
          (diamonds (alist-val :diamonds mapping))
          (jokers   (alist-val :joker mapping)))
      (is eq 1 (length jokers))
      (is eq 1 (->> (list spades hearts clubs diamonds)
                    (mapcar #'length)
                    remove-duplicates
                    length)))))
