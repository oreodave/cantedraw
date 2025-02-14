;;; model.lisp - 2025-02-14

;; Copyright (C) 2025 Aryadev Chavali

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License Version 2 for
;; details.

;; You may distribute and modify this code under the terms of the GNU General
;; Public License Version 2, which you should have received a copy of along with
;; this program.  If not, please go to <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Model of cards and sets of cards in Lisp.

;;; Code:

(in-package :cantedraw.model)

(deftype rank ()
  `(or (member :Jack :Queen :King :Ace)
       (integer 2 10)))

(deftype suit ()
  `(member :Diamonds :Clubs :Hearts :Spades :Joker))

(deftype card () `(cons rank suit))

(deftype int-card () `(integer 0 51))

(fn cardsetp (lst) (-> (list) boolean)
  (every #'(lambda (x) (typep x 'card)) lst))

(deftype cardset () `(and list (satisfies cardsetp)))

(fn int->suit (n) (-> (int-card) suit)
  (case (floor n 13)
    (0 :Diamonds)
    (1 :Clubs)
    (2 :Hearts)
    (3 :Spades)
    (t :Joker)))

(fn suit->int (item) (-> (suit) (integer 0 4))
  (case item
    (:Diamonds 0)
    (:Clubs    1)
    (:Hearts   2)
    (:Spades   3)
    (t         4)))

(fn int->rank (n) (-> (int-card) rank)
  (case (mod n 13)
    (0  :Ace)
    (10 :Jack)
    (11 :Queen)
    (12 :King)
    (t  (+ n 1))))

(fn rank->int (rank) (-> (rank) (integer 0 12))
  (case rank
    (:Ace    0)
    (:Jack  10)
    (:Queen 11)
    (:King  12)
    (t      (- rank 1))))

(fn int->card (num) (-> (int-card) card)
  (cons (int->rank num)
        (int->suit num)))

(fn card->int (card) (-> (card) int-card)
  (destructuring-bind (rank . suit) card
    (->> (suit->int suit)
         (* 13)
         (+ (rank->int rank)))))
