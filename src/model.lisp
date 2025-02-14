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

(fn int->card (num) (-> (int-card) card)
  (cons (int->rank num)
        (int->suit num)))

(fn card->int (card) (-> (card) int-card)
  (destructuring-bind (rank . suit) card
    (->> (suit->int suit)
         (* 13)
         (+ (rank->int rank)))))

(fn suit< (s1 s2) (-> (suit suit) boolean)
  (< (suit->int s1) (suit->int s2)))

(fn rank< (r1 r2) (-> (rank rank) boolean)
  (< (rank->int r1) (rank->int r2)))

(fn card< (c1 c2) (-> (card card) boolean)
  (destructuring-bind ((r1 . s1) (r2 . s2)) (list c1 c2)
    (if (eq r1 r2)
        (suit< s1 s2)
        (rank< r1 r2))))

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

(fn card->str (card) (-> (card) string)
  (destructuring-bind (rank . suit) card
    (if (eq suit :Joker)
        "Joker"
        (format nil "~a[~a]"
                (rank->str rank)
                (suit->str suit)))))

(fn cardset->str (cardset) (-> (cardset) string)
  (->> cardset
       (mapcar #'card->str)
       (format nil "~{~a~^, ~}")))

(fn make-joker (&optional (rank :ACE)) (-> (&optional rank) card)
  (cons rank :Joker))

(fn make-deck (&optional (n 1)) (-> (&optional fixnum) cardset)
  (append
   (loop :for _ :from 1 :to n
         :nconc (loop :for j :from 1 :to 52
                      collect (int->card (1- j))))
   (mapcar ($ int->rank make-joker)
      (range 0 (* 2 n)))))
