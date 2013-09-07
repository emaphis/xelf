;;; shell.lisp --- editor ui

;; Copyright (C) 2012, 2013  David O'Toole

;; Author: David O'Toole <dto@xelf.io>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :xelf)

;;; Message output widget

(define-block messenger :category :terminal :messages nil)

(define-method initialize messenger (&optional messages)
  (cond 
    ((stringp messages)
     (setf %messages (list messages)))
    ((consp messages)
     (setf %messages messages))))

(define-method add-message messenger (message-string)
  (assert (stringp message-string))
  (push message-string %messages))

(defparameter *messenger-columns* 80)
(defparameter *messenger-rows* 7)

(define-method get-messages messenger ()
  (or %messages *message-history*))

(define-method layout messenger ()
  (setf %height (+ (* (font-height *font*) *messenger-rows*)
		   (dash 4)))
  (let ((width 0))
    (block measuring
      (dotimes (n *messenger-rows*)
	(if (<= (length (get-messages self)) n)
	    (return-from measuring nil)
	    (setf width 
		  (max width 
		       (font-text-width 
			(nth n (get-messages self))
			*block-font*))))))
    (setf %width (+ width (dash 5)))))
			     
(define-method draw messenger ()
  (draw-background self)
  (with-fields (x y width height) self
      (let ((y0 (+ y height (- 0 (font-height *font*) (dash 2))))
	    (x0 (+ x (dash 3))))
	(dotimes (n *messenger-rows*)
	  (unless (<= (length (get-messages self)) n)
	    (draw-string (nth n (get-messages self))
			 x0 y0
			 :color "gray70"
			 :font *block-font*)
	    (decf y0 (font-height *font*)))))))

;;; Modeline

(defun-memo modeline-position-string (x y)
    (:key #'identity :test 'equal :validator #'identity)
  (format nil "X:~S Y:~S" x y))

(defun-memo modeline-database-string (local global)
    (:key #'identity :test 'equal :validator #'identity)
  (format nil "~S/~S objects" local global))

(define-block-macro modeline
    (:super phrase
     :fields 
     ((orientation :initform :horizontal)
      (no-background :initform t)
      (spacing :initform 4))
     :inputs (:buffer-id (new 'label :read-only t)
	      :position (new 'label :read-only t)
	      :mode (new 'label :read-only t)
	      :objects (new 'label :read-only t)
	      :project-id (new 'label :read-only t))))

(define-method update modeline ()
  (mapc #'pin %inputs)
  (set-value %%project-id *project*)
  (set-value %%buffer-id (or (%buffer-name (current-buffer)) "*untitled-buffer*"))
  (set-value %%objects (modeline-database-string (hash-table-count (%objects (current-buffer)))
						 (hash-table-count *database*)))
  (set-value %%position
	     (modeline-position-string
	      (%window-x (current-buffer))
	      (%window-y (current-buffer))))
  (set-value %%mode
	     (if (current-buffer)
		 (if (field-value :paused (current-buffer))
		     "(paused)"
		     "(playing)")
		 "(empty)")))

;;; Shell prompt

(define-block (shell-prompt :super entry)
  (result :initform nil)
  (background :initform nil))

(define-method can-pick shell-prompt () nil)

(define-method pick shell-prompt ()
  nil)

(define-method enter shell-prompt (&optional no-clear)
  (prompt%enter self)
  (with-fields (result) self
    (replace-contents *shell*
			(list 
			 (if (xelfp result) 
			     result (make-phrase result))))))


(define-method lose-focus shell-prompt ()
  (cancel-editing self))

(define-method evaluate-expression shell-prompt (sexp)
  (with-fields (result) self
    (setf result (eval (first sexp)))))

;;; The shell is a command prompt and message output area.

(defparameter *minimum-shell-width* 400)
(defparameter *shell-background-color* "gray20")

(define-block-macro shell
    (:super phrase
     :fields 
     ((orientation :initform :vertical)
      (frozen :initform t)
      (spacing :initform 4))
     :inputs
     (:modeline (new 'modeline)
      :contents (new 'phrase (new 'messenger))
      :prompt (new 'shell-prompt))))

(define-method replace-contents shell (items)
  (unfreeze %%contents)
  (mapc #'destroy (%inputs %%contents))
  (dolist (item items)
    (accept %%contents item))
  (freeze %%contents))

(define-method hit shell (x y)
  (when (within-extents x y %x %y (+ %x %width) (+ %y %height))
    self))

(define-method tap shell (x y)
  (focus self))

(define-method alternate-tap shell (x y) nil)

(define-method get-prompt shell () %%prompt)

(defun shell-prompt ()
  (when *shell* (get-prompt *shell*)))

(define-method focus shell ()
  (let ((prompt (get-prompt self)))
    (set-read-only prompt nil)
    (grab-focus prompt)))
	;;
;;	(replace-contents self (list result))))))




;;; shell.lisp ends here
