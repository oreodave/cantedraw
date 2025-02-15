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
  (:use :cl)
  (:export
   :--> :->>
   :-> :fn
   :while :alist-val
   :$))

(defpackage cantedraw.lib.functions
  (:use :cl :cantedraw.lib.macros)
  (:export
   :parse-integer*
   :range :take :split
   :rev-map
   :remove-at-indices))

(defpackage cantedraw.model
  (:use :cl :cantedraw.lib.macros :cantedraw.lib.functions)
  (:export
   ;; Types
   :int-card :rank :suit :card :cardset
   ;; Converters
   :int->suit :int->rank :int->card
   :suit->int :rank->int :card->int
   ;; Comparators
   :suit< :rank< :card<
   ;; Serialisers
   :suit->str :rank->str :card->str :cardset->str
   ;; Constructors
   :make-joker :make-deck))

(defpackage cantedraw.player
  (:use :cl
   :cantedraw.lib.macros :cantedraw.lib.functions
   :cantedraw.model)
  (:export
   :player
   :players
   :error-player-nonexistent
   :error-player-broke
   :player-id :player-balance :player-hand
   :player-exists? :player-bankrupt? :player-can-bet?
   :player-pay
   :player-receive))

(defpackage cantedraw.game
  (:use :cl
   :cantedraw.lib.macros :cantedraw.lib.functions
   :cantedraw.model)
  (:export
   :deal-cards
   :deal-hands
   :redeal-hand))

(defpackage cantedraw.main
  (:use :cl
   :cantedraw.lib.macros :cantedraw.lib.functions
   :cantedraw.model :cantedraw.game)
  (:export :start))
