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

(define-test (model-test int->rank)
  :depends-on ((cantedraw/tests/macros ->>)
               (cantedraw/tests/functions rev-map))
  :compile-at :execute
  (fail (int->rank nil))
  (fail (int->rank "Not a number"))
  ;; Proving int->rank maps 0-51 to produces 13 ranks, all equally distributed.
  (let ((mapping (rev-map #'int->rank (range 0 52))))
    ;; Prove there are 13 ranks
    (is eq 13 (length mapping))
    ;; Prove every rank is equivalent in length.
    (is eq 1 (->> mapping
                  (mapcar ($>> cdr length))
                  remove-duplicates
                  length))
    ;; Prove Ace, 2, ..., 10, Jack, Queen, King are the 13 ranks.
    (true (every #'identity
             (->> (list :ace :king :queen :jack)
                  (append (range 2 11))
                  (mapcar (lambda (rank) (assoc rank mapping))))))))

(define-test (model-test rank->int)
  :depends-on ((cantedraw/tests/macros ->>))
  :compile-at :execute
  (fail (rank->int nil))
  (fail (rank->int 1738))
  (fail (rank->int "not a rank"))
  (fail (rank->int :still-not-a-rank))
  ;; Prove ranks are mapped to unique positive integers
  (let ((res (->> (list :jack :queen :king :ace)
                  (append (range 2 11))
                  (mapcar #'rank->int))))
    (true (every #'integerp res))
    (true (every #'(lambda (x) (<= 0 x)) res))
    (is equal (length res) (length (remove-duplicates res)))))

(define-test (model-test "int->rank and rank->int are complete inverses")
  :depends-on (rank->int int->rank)
  (let ((int-range (range 0 13))
        (rank-range (->> (list :ace :jack :queen :king)
                         (append (range 2 11)))))
    (is equal rank-range
        (mapcar ($>> rank->int int->rank) rank-range))
    (is equal int-range
        (mapcar ($>> int->rank rank->int) int-range))))

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

(define-test (model-test suit->int)
  :depends-on ((cantedraw/tests/macros ->>))
  :compile-at :execute
  (fail (suit->int nil))
  (fail (suit->int "not a suit"))
  (fail (suit->int :still-not-a-suit))
  (fail (suit->int 42069))
  ;; Prove suits are mapped to unique positive integers
  (let ((res (->> (list :diamonds :clubs :hearts :spades :joker)
                  (mapcar #'suit->int))))
    (true (every #'integerp res) "All integers")
    (true (every ($>> (<= 0)) res) "All positive")
    (is equal (length res) (length (remove-duplicates res)) "Unique mapping")))

(define-test (model-test "int->suit and suit->int are complete inverses")
  :depends-on (suit->int int->suit)
  (let ((int-range (list 0 13 26 39 52))
        (suit-range (list :diamonds :clubs :hearts :spades :joker)))
    (is equal suit-range
        (mapcar ($>> suit->int int->suit) suit-range))
    (is equal int-range
        (mapcar ($>> int->suit suit->int) int-range))))

(define-test (model-test int->card)
  :depends-on ((cantedraw/tests/functions range))
  :compile-at :execute
  (fail (int->card nil))
  (fail (int->card "Not a number"))
  ;; Proving int->card maps 0-51 to exactly 52 unique cards
  (let ((mapping (mapcar #'int->card (range 0 52))))
    (is eq 52 (length (remove-duplicates mapping :test #'equal))
        "52 unique elements.")
    (true (every #'card-p mapping)
          "Every element is a card."))
  ;; Prove that cards outside of [0, 51] are mapped to jokers (not exhaustive)
  (loop :for positive :from 100 :to 200
        :for negative :from -200 :to -100
        :for inp := (mapcar #'int->card (list positive negative))
        :do (true (every #'card-p inp)
                  "Is a card.")
        :do (true (every (lambda (c) (eq :joker (card-suit c))) inp)
                  "Are jokers.")))

(define-test (model-test card->int)
  :depends-on ((cantedraw/tests/functions range))
  :compile-at :execute
  (fail (card->int nil))
  (fail (card->int 1738))
  (fail (card->int "not a card"))
  (fail (card->int :still-not-a-card))
  (let ((ranks (append (range 2 11) (list :jack :queen :king :ace))))
    (let ((res (->> (loop :for suit :in (list :diamonds :clubs :hearts :spades)
                          :nconc
                          (mapcar (lambda (rank) (make-card :rank rank :suit suit)) ranks))
                    (mapcar #'card->int))))
      (true (every #'integerp res) "Every mapped element is an integer.")
      (true (every #'(lambda (x) (<= 0 x)) res) "Every mapped element is positive")
      (is eq (length res) (length (remove-duplicates res))
          "All mapped integers are unique."))
    (let ((res (->> (loop :for rank :in ranks
                          :collect (make-card :rank rank :suit :joker))
                    (mapcar #'card->int))))
      (true (every #'integerp res) "Every mapped element is an integer.")
      (true (every (lambda (n) (or (< n 0) (> n 51))) res)
            "Every mapped element is outside of [0,51]")
      (is eq (length res) (length (remove-duplicates res))
          "All mapped integers are unique."))))
