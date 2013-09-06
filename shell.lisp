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

(defparameter *shell-menu-entries*
  '((:label "Project"
     :inputs
     ((:label "Create a new project" :action :create-project)
      (:label "Save current project" :action :save-project)
      (:label "Load a project" :action :load-project)
      (:label "Settings" :action :settings)
      (:label "Quit Xelf" :action :quit-xelf)))
    (:label "Buffer"
     :inputs
     ((:label "Cut" :action :cut)
      (:label "Copy" :action :copy)
      (:label "Paste" :action :paste)
      (:label "Paste as new buffer" :action :paste-as-new-buffer)
      (:label "Select all" :action :select-all)
      (:label "Clear selection" :action :clear-selection)))
    ;; (:label "View"

    (:label "Block"
     :inputs
     ((:label "Inspect" :action :inspect)
      (:label "Copy" :action :do-copy)
      (:label "Destroy" :action :destroy)))
    (:label "Help"
     :inputs
     ((:label "Copyright notice" :action :show-copyright-notice)
      (:label "General help" :action :general-help)
      (:label "Examples" :action :show-examples)
      (:label "Language Reference" :action :language-reference)))))

;;; A basic action button

(define-block (button :super phrase)
  (category :initform :button)
  (target :initform nil)
  (method :initform nil)
  (arguments :initform nil)
  (label :initform nil))

(define-method initialize button 
    (&key target method arguments label)
  (when target (setf %target target))
  (when method (setf %method method))
  (when label (setf %label label))
  (when arguments (setf %arguments arguments)))

(define-method layout button ()
  (with-fields (height width) self
    (setf width (+ (* 13 (dash))
		   (font-text-width %label
				    *block-bold*))
	  height (+ (font-height *block-bold*) (* 4 (dash))))))

(define-method draw button ()
  (with-fields (x y height width label) self
    (with-style :rounded (draw-patch self x y (+ x width) (+ y height)))
    (draw-image "colorbang" 
		    (+ x (dash 1))
		    (+ y (dash 1)))
    (draw-string %label (+ x (dash 9)) (+ y (dash 2))
		 :color "white"
		 :font *block-bold*)))

(define-method tap button (x y)
  (apply #'send %method %target %arguments))

;;; Headline 

(define-block headline title)

(defparameter *xelf-title-string* "Xelf 0.92a")

(define-method initialize headline (&optional (title *project*))
  (initialize%super self)
  (setf %title title))

(define-method layout headline ()
  (resize self 
	  (+ (dash 2) *logo-height*)
	  *logo-height*))

(define-method draw headline ()
  (with-fields (x y) self
    (draw-image "xelf" (+ x (dash 0.5)) (- y (dash 0.5)) :height *logo-height* :width *logo-height*)
    (draw-string %title
		 (+ x *logo-height* (dash 2))
		 (+ y (dash 1))
		 :color "white"
		 :font *block-bold*)))

;;; Generic window titlebar utility

(define-block (window :super phrase)
  (centered :initform nil)
  (tags :initform '(:window))
  (category :initform :system))

(defun windowp (thing)
  (and (xelfp thing)
       (has-tag thing :window)))

(define-method initialize window (&key child (title "*untitled-window*"))
  (assert child)
  (initialize%super self)
  (setf %inputs (list (new 'headline title) child))
  (update-parent-links self)
  (mapc #'pin %inputs))

(define-method layout window ()
  (layout-vertically self)
  (align-to-pixels self))

(define-method can-pick window ()
  t)

(define-method pick window ()
  self)

(define-method center window ()
  (layout self)
  (center%super self))

(define-method accept window (thing))

(define-method draw-hover window ())

(define-method after-unplug-hook window (thing)
  (destroy self))

;;; Shell prompt

(define-block (shell-prompt :super entry)
  (background :initform nil)
  output)

(defun debug-on-error ()
  (setf *debug-on-error* t))

(defun print-on-error ()
  (setf *debug-on-error* nil))

(define-method set-output shell-prompt (output)
  (setf %output output))

(define-method can-pick shell-prompt () nil)

(define-method pick shell-prompt ()
  %parent)

(define-method enter shell-prompt (&optional no-clear)
  (prompt%enter self))

(define-method do-sexp shell-prompt (sexp)
  (with-fields (output) self
    (assert output)
    (let ((container output)
	  (result nil))
      (assert (xelfp container))
      ;; output messages too
      (let ((*message-function* 
	      #'(lambda (text)
		  (format t "~A" text)
		  (accept container (new 'label :line text)))))
	;; execute lisp expressions
	(setf result (eval (first sexp)))
	;; ;; we didn't crash, at least. output the reusable sexp
	;; (accept container (new 'expression :line %last-line))
	;; do something with result
	(let ((new-block 
		(if (xelfp result)
		    result
		    (new 'expression :value result))))
      	  ;; spit out result block, if any
      	  (when new-block 
      	    (accept container new-block)))))))

(define-method lose-focus shell-prompt ()
  (cancel-editing self))

(define-method do-after-evaluate shell-prompt ()
  ;; print any error output
  (when (and %output (stringp %error-output)
	     (plusp (length %error-output)))
    (accept %output (new 'text %error-output))))

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


;; ;;; The Shell is a command prompt and message output area.

(define-block-macro shell
    (:super phrase
     :fields 
     ((orientation :initform :vertical)
      (no-background :initform t)
      (spacing :initform 4))))

(defparameter *minimum-shell-width* 200)

(define-method initialize shell ()
  (with-fields (image inputs) self
    (let ((prompt (new 'shell-prompt))
	  (modeline (new 'modeline)))
      (initialize%super self)
      (set-output prompt self)
      (setf inputs (list modeline prompt))
      (set-parent prompt self)
      (set-parent modeline self)
      (pin prompt)
      (pin modeline))))

(define-method layout shell ()
  (with-fields (height width parent inputs) self
    (setf height 0)
    (setf width 0)
    ;; update all child dimensions
    (dolist (element inputs)
      (layout element)
      (incf height (field-value :height element))
      (callf max width (dash 2 (field-value :width element))))
    ;; now compute proper positions and re-layout
    (let* ((x (+ (dash 1) (%window-x (current-buffer))))
	   (y0 (+ (%window-y (current-buffer))
		  *gl-screen-height*))
	   (y (- y0 height (dash 3))))
      (dolist (element inputs)
	(decf y0 (field-value :height element))
	(move-to element x y0)
	(layout element))
      (setf %y y)
      (setf %x x)
      ;;  a little extra room at the top and sides
      (incf height (dash 3)))))
;; ;; move to the right spot to keep the bottom on the bottom.
      ;; (setf y (- y0 (dash 1))))))

;; (define-method update shell ()
;;   (update (first %inputs))
;;   (update (second %inputs)))

(define-method get-prompt shell ()
  (second %inputs))

(defun shell-prompt ()
  (when *shell* (get-prompt *shell*)))

(define-method enter shell ()
  (enter (get-prompt self)))
 
(define-method evaluate shell ()
  (evaluate (get-prompt self)))

(define-method focus shell ()
  (let ((prompt (get-prompt self)))
    (set-read-only prompt nil)
    (grab-focus prompt)))

(defparameter *shell-background-color* "gray20")

(define-method draw shell ()
  (with-fields (inputs x y height width) self
    (draw-box (window-x) y *gl-screen-width* height :color *shell-background-color*)
    (mapc #'draw inputs)))

(define-method hit shell (x y)
  (when (within-extents x y %x %y (+ %x %width) (+ %y %height))
    self))

;; (define-method accept shell (thing)
;;   (destroy (third %inputs))
;;   (setf (third

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
     :inputs (:project-id (new 'label :read-only t)
	      :buffer-id (new 'label :read-only t)
	      :position (new 'label :read-only t)
	      :mode (new 'label :read-only t)
	      :objects (new 'label :read-only t))))

(define-method update modeline ()
  (mapc #'pin %inputs)
  (set-value %%project-id *project*)
  (set-value %%buffer-id (or (%buffer-name (current-buffer)) "nil"))
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

;;; shell.lisp ends here
