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
