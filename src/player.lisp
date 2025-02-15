;;; player.lisp - 2025-02-15

;; Copyright (C) 2025 Aryadev Chavali

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License Version 2 for
;; details.

;; You may distribute and modify this code under the terms of the GNU General
;; Public License Version 2, which you should have received a copy of along with
;; this program.  If not, please go to <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Player management - in particular, financing and hand management.

;;; Code:

(in-package :cantedraw.player)

(defstruct player
  (name   "Yunkyu" :type string)
  (balance 0       :type integer)
  (hand    nil       :type (or nil cardset)))

(define-condition error-player-nonexistent (error)
  ((id :initarg :id :reader id))
  (:report
   (lambda (err stream)
     (format stream "Player [~a] is non-existent or malformed" (id err)))))

(define-condition error-player-broke (error)
  ((id :initarg :id :reader id)
   (balance :initarg :balance :reader balance)
   (required :initarg :required :reader required))
  (:report
   (lambda (err stream)
     (format stream "Player [~a] has balance $~a but $~a requested."
             (id err) (balance err) (required err)))))

(fn player-exists? (id table) (-> (symbol hash-table) boolean)
  (if (or (null (gethash id table))
          (not (player-p (gethash id table))))
      (error 'error-player-nonexistent :id id)
      t))

(fn player-can-pay? (id table amount) (-> (symbol hash-table fixnum) boolean)
  (player-exists? id table)
  (->> (gethash id table) player-balance (<= amount)))

(fn player-bankrupt? (id table) (-> (symbol hash-table) boolean)
  (player-exists? id table)
  (->> (gethash id table) player-balance (>= 0)))

(fn player-debit (id table amount) (-> (symbol hash-table fixnum) fixnum)
  (player-exists? id table)
  (if (not (player-can-pay? id table amount))
      (error 'error-player-broke
             :id id :balance (player-balance (gethash id table)) :required amount))
  (decf (player-balance (gethash id table)) amount))

(fn player-credit (id table amount) (-> (symbol hash-table fixnum) fixnum)
  (player-exists? id table)
  (incf (player-balance (gethash id table)) amount))

(fn player-set-cards (id table cards) (-> (symbol hash-table cardset) t)
  (player-exists? id table)
  (setf (player-hand (gethash id table)) cards))
