;;; game.lisp - 2025-02-14

;; Copyright (C) 2025 Aryadev Chavali

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License Version 2 for
;; details.

;; You may distribute and modify this code under the terms of the GNU General
;; Public License Version 2, which you should have received a copy of along with
;; this program.  If not, please go to <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Game mechanic code.

;;; Code:

(in-package :cantedraw.game)

(fn deal-cards (n deck) (-> (fixnum cardset) (cons cardset cardset))
  (destructuring-bind (hand . rest) (split n deck)
    (cons (sort hand #'card<) rest)))

(fn deal-hands (n deck) (-> (fixnum cardset) (cons list cardset))
  (if (< (length deck) (* 5 n))
      (error "Require at least ~a cards for ~a hands, but have ~a cards!"
             n (* 5 n) (length deck)))
  (loop :for i :from 1 :to n
        :for (hand . rest) = (deal-cards 5 deck) :then (deal-cards 5 rest)
        :collect hand :into x
        :finally (return (cons x rest))))

(fn redeal-hand (hand indices deck) (-> (cardset list cardset)
                                        (cons cardset cardset))
  (cond
    ((null indices) (cons hand deck))
    ((= 5 (length indices)) (deal-cards 5 deck))
    (t
     (destructuring-bind (new-cards . deck)
         (deal-cards (length indices) deck)
       (--> it
         (remove-at-indices indices hand)
         (append it new-cards)
         (sort it #'card<)
         (cons it deck))))))
