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

