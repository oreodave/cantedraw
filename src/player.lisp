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

(defun playerp (x)
  (and (typep (car x) 'fixnum)
       (typep (cadr x) 'fixnum)
       (typep (caddr x) 'cardset)))

(deftype player () `(and cons (satisfies playerp)))

(deftype players () 'list)

(define-condition error-player-nonexistent (error)
  ((id :initarg :id :reader id))
  (:report
   (lambda (err stream)
     (format stream "Player [~a] is non-existent or malformed"
             (id err)))))

(define-condition error-player-broke (error)
  ((id :initarg :id :reader id)
   (balance :initarg :balance :reader balance)
   (required :initarg :required :reader required))
  (:report
   (lambda (err stream)
     (format stream "Player [~a] has balance $~a but $~a requested."
             (id err)
             (balance err)
             (required err)))))


(fn player-id (player) (-> (player) fixnum)
  (car player))

(fn player-balance (player) (-> (player) fixnum)
  (cadr player))

(fn player-hand (player) (-> (player) cardset)
  (caddr player))

(fn player-exists? (id players) (-> (fixnum players) boolean)
  (assoc id players))

(fn player-bankrupt? (player) (-> (player) boolean)
  (<= (cadr player) 0))

(fn player-can-bet? (min-bet player) (-> (fixnum player) boolean)
  (>= (cadr player) min-bet))

(fn player-pay (id amount players) (-> (fixnum fixnum players) players)
  (let ((p (assoc id players)))
    (cond
      ((not (typep p 'player))
       (error 'error-player-nonexistent
              :id id))
      ((not (player-can-bet? amount p))
       (error 'error-player-broke
              :id id
              :balance (player-balance p)
              :required amount))
      (t
       (destructuring-bind (id balance cards) p
         (setf (cdr (assoc id players))
               (list (- balance amount) cards)))))
    players))

(fn player-receive (id amount players)
    (-> (fixnum fixnum players) players)
  (let ((p (assoc id players)))
    (if (not (typep p 'player))
        (error 'error-player-nonexistent
               :id id))
    (destructuring-bind (id balance cards) p
      (setf (cdr (assoc id players))
            (list (+ balance amount) cards)))
    players))
