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

(defstruct card
  (rank :Ace   :type rank)
  (suit :Joker :type suit))

(deftype int-card () `(integer 0 51))

(deftype cardset () `(and list (satisfies cardsetp)))

(fn cardsetp (lst) (-> (list) boolean)
  (every #'card-p lst))

(fn int->suit (n) (-> (fixnum) suit)
  (case (floor n 13)
    (0 :Diamonds)
    (1 :Clubs)
    (2 :Hearts)
    (3 :Spades)
    (t :Joker)))

(fn suit->int (item) (-> (suit) (integer 0 52))
  (case item
    (:Diamonds 0)
    (:Clubs    13)
    (:Hearts   26)
    (:Spades   39)
    (t         52)))

(fn int->rank (n) (-> (fixnum) rank)
  (let ((n (mod n 13)))
    (case n
      (9 :Jack)
      (10 :Queen)
      (11 :King)
      (12  :Ace)
      (t  (+ n 2)))))

(fn rank->int (rank) (-> (rank) (integer 0 12))
  (case rank
    (:Jack  9)
    (:Queen 10)
    (:King  11)
    (:Ace    12)
    (t      (- rank 2))))

(fn int->card (num) (-> (fixnum) card)
  (make-card :rank (int->rank num)
             :suit (int->suit num)))

(fn card->int (card) (-> (card) fixnum)
  (with-slots ((rank rank) (suit suit)) card
    (let ((rank (rank->int rank))
          (suit (suit->int suit)))
      (+ rank suit))))

(fn suit< (s1 s2) (-> (suit suit) boolean)
  (< (suit->int s1) (suit->int s2)))

(fn rank< (r1 r2) (-> (rank rank) boolean)
  (< (rank->int r1) (rank->int r2)))

(fn card< (c1 c2) (-> (card card) boolean)
  (with-slots ((r1 rank) (s1 suit)) c1
    (with-slots ((r2 rank) (s2 suit)) c2
      (cond
        ;; Check for jokers!
        ((and (eq s1 :Joker)
              (eq s2 :Joker))
         (rank< r1 r2))
        ((eq s1 :Joker) nil)
        ((eq s2 :Joker) t)
        ((eq r1 r2) (suit< s1 s2))
        (t (rank< r1 r2))))))

(fn suit->str (suit) (-> (suit) string)
  (case suit
    (:Diamonds "◆")
    (:Clubs    "♣")
    (:Hearts   "♥")
    (:Spades   "♠")
    (t         "Joker")))

(fn rank->str (rank) (-> (rank) string)
  (case rank
    (:Ace   "Ace")
    (:Jack  "Jack")
    (:Queen "Queen")
    (:King  "King")
    (t      (format nil "~a" rank))))

(defmethod print-object ((card card) s)
  (with-slots ((rank rank) (suit suit)) card
    (if (eq suit :Joker)
        (format s "Joker")
        (format s "~a[~a]"
                (rank->str rank)
                (suit->str suit)))))

(fn cardset->str (cardset) (-> (cardset) string)
  (format nil "~{~a~^, ~}" cardset))

(fn make-deck (&optional (n 1)) (-> (&optional fixnum) cardset)
  (append
   (loop :for _ :from 1 :to n
         :nconc (loop :for j :from 1 :to 52
                      collect (int->card (1- j))))
   (loop :for _ :from 1 :to (* 2 n)
         :collect (make-card))))
