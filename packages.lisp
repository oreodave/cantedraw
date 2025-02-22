;;; packages.lisp - 2025-02-09

;; Copyright (C) 2025 Aryadev Chavali

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License Version 2 for
;; details.

;; You may distribute and modify this code under the terms of the GNU General
;; Public License Version 2, which you should have received a copy of along with
;; this program.  If not, please go to <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Define the packages (namespaces) involved in this project.

;;; Code:

(defpackage cantedraw.lib.macros
  (:nicknames :5d-lib.macros)
  (:use :cl)
  (:export
   :--> :->> :-<>
   :-> :fn
   :while :alist-val
   :$-> :$>> :$<>))

(defpackage cantedraw.lib.functions
  (:nicknames :5d-lib.functions)
  (:use :cl :5d-lib.macros)
  (:export
   :parse-integer*
   :range :take :split
   :rev-map
   :remove-at-indices))

(defpackage cantedraw.model
  (:nicknames :5d.model)
  (:use :cl :5d-lib.macros :5d-lib.functions)
  (:export
   ;; Types
   :int-card :rank :suit :cardset
   ;; card struct
   :card :make-card :card-suit :card-rank :card-p
   ;; Converters
   :int->suit :int->rank :int->card
   :suit->int :rank->int :card->int
   ;; Comparators
   :suit< :rank< :card<
   ;; Serialisers
   :suit->str :rank->str :cardset->str
   ;; Constructors
   :make-deck))

(defpackage cantedraw.player
  (:nicknames :5d.player)
  (:use :cl
   :5d-lib.macros :5d-lib.functions
   :5d.model)
  (:export
   :player
   :error-player-nonexistent
   :error-player-broke
   :player-exists? :player-bankrupt? :player-can-pay?
   :player-debit
   :player-credit
   :player-set-cards))

(defpackage cantedraw.game
  (:nicknames :5d.game)
  (:use :cl
   :5d-lib.macros :5d-lib.functions
   :5d.model)
  (:export
   :deal-cards
   :deal-hands
   :redeal-hand))

(defpackage cantedraw.main
  (:nicknames :5d.main)
  (:use :cl
   :5d-lib.macros :5d-lib.functions
   :5d.model :5d.game)
  (:export :start))
