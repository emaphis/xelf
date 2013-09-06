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

;;; Custom data entry for Minibuffer. See also entry.lisp 

(define-block (minibuffer-prompt :super entry)
  (background :initform nil)
  output)

(defun debug-on-error ()
  (setf *debug-on-error* t))

(defun print-on-error ()
  (setf *debug-on-error* nil))

(define-method set-output minibuffer-prompt (output)
  (setf %output output))

(define-method can-pick minibuffer-prompt () nil)

(define-method pick minibuffer-prompt ()
  %parent)

(define-method enter minibuffer-prompt (&optional no-clear)
  (prompt%enter self))

(define-method evaluate-here minibuffer-prompt ()
  (prompt%enter self))

(define-method do-sexp minibuffer-prompt (sexp)
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

(define-method lose-focus minibuffer-prompt ()
  (cancel-editing self))

(define-method do-after-evaluate minibuffer-prompt ()
  ;; print any error output
  (when (and %output (stringp %error-output)
	     (plusp (length %error-output)))
    (accept %output (new 'text %error-output))))

;;; The Minibuffer is a command prompt and message output area.

(define-block-macro minibuffer
    (:super phrase
     :fields 
     ((orientation :initform :vertical)
      (no-background :initform t)
      (spacing :initform 4))
     :inputs 
     (:modeline (new 'modeline)
      :prompt (new 'minibuffer-prompt))))

(defparameter *minimum-minibuffer-width* 200)

(define-method initialize minibuffer ()
  (with-fields (image inputs) self
    (let ((prompt (new 'minibuffer-prompt))
	  (modeline (new 'modeline)))
      (initialize%super self)
      (set-output prompt self)
      (setf inputs (list modeline prompt))
      (set-parent prompt self)
      (set-parent modeline self)
      ;; (setf %sidebar (new 'sidebar))
      (pin prompt)
      (pin modeline))))

(define-method layout minibuffer ()
  ;; (layout %sidebar)
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

(define-method update minibuffer ()
  (update (first %inputs))
  (update (second %inputs)))

(define-method get-prompt minibuffer ()
  (second %inputs))

(defun minibuffer-prompt ()
  (when *minibuffer* (get-prompt *minibuffer*)))

(define-method enter minibuffer ()
  (enter (get-prompt self)))
 
(define-method evaluate minibuffer ()
  (evaluate (get-prompt self)))

(define-method focus minibuffer ()
  (let ((prompt (get-prompt self)))
    (set-read-only prompt nil)
    (grab-focus prompt)))

(defparameter *minibuffer-rows* 10)

(define-method accept minibuffer (input &optional prepend)
  (declare (ignore prepend))
  (with-fields (inputs) self
    (assert (not (null inputs))) ;; we always have a prompt
    (prog1 t
      (assert (valid-connection-p self input))
      ;; set parent if necessary 
      (adopt self input)
      (setf inputs 
	    (concatenate 'list
			 (list (first inputs) 
			       (second inputs)
			       input)
			 (nthcdr 2 inputs)))
      ;; drop last item in scrollback
      (let ((len (length inputs)))
	(when (> len *minibuffer-rows*)
	  (dolist (item (subseq inputs *minibuffer-rows*))
	    (when (xelfp item) (destroy item)))
	  (setf inputs (subseq inputs 0 (1- len))))))))
      ;; 
;;      (add-item %sidebar (duplicate-safely input)))))

(defparameter *minibuffer-background-color* "gray20")

(define-method draw minibuffer ()
  (with-fields (inputs x y height width) self
    (draw-box (window-x) y *gl-screen-width* height :color *minibuffer-background-color*)
    (mapc #'draw inputs)))
;;    (draw %sidebar)))

(define-method hit minibuffer (x y)
  ;; (or (hit %sidebar x y)
     (when (within-extents x y %x %y (+ %x %width) (+ %y %height))
	self))

;;;;;;;;;;;;;;;;;;;;;

;;; A basic action button

(define-block (button :super :list)
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

;;; A block representing the current system and project universe

(defparameter *shell-menu-entries*
  '((:label "Project"
     :inputs
     ((:label "Create a new project" :action :create-project)
      (:label "Save current project" :action :save-project)
      (:label "Load a project" :action :load-project)
      ;; (:label "Show current changes without saving" :action :show-changes)
      ;; (:label "Export as archive" :action :export-archive)
      ;; (:label "Export as application" :action :export-application)
      ;; (:label "Publish to web" :action :publish-web)
      ;; (:label "Publish to community site" :action :publish-community)
      ;; (:label "Publish to FTP" :action :publish-ftp)
      (:label "Settings" :action :settings)
      (:label "Quit Xelf" :action :quit-xelf)))
    (:label "Edit"
     :inputs
     ((:label "Cut" :action :cut)
      (:label "Copy" :action :copy)
      (:label "Paste" :action :paste)
      (:label "Paste in new workspace" :action :paste-as-new-workspace)
      (:label "Select all" :action :select-all)
      (:label "Clear selection" :action :clear-selection)))
    (:label "Blocks"
     :inputs
     ((:label "Define a block" :action :open-define-block-dialog)
      (:label "Define a method" :action :open-define-method-dialog)
      (:label "Inspect" :action :inspect)
      (:label "Clone" :action :do-clone)
      (:label "Copy" :action :do-copy)
      (:label "Destroy" :action :destroy)))
    (:label "Resources"
     :inputs
     ((:label "Import new resource" :action :import-resources)
      (:label "Edit resource" :action :edit-resource)
      (:label "Search resources" :action :search-resources)
      (:label "Export resource(s)" :action :export-resources)
      (:label "Browse resources" :action :browse-resources)))
    ;; (:label "Tools" 
    ;;  :inputs
    ;;  ((:label "Create a Lisp listener" :action :create-listener)
    ;;   (:label "Create a text box" :action :create-text)
    ;;   (:label "Create a trash can" :action :create-trash)))
    (:label "Workspace" :inputs
     ((:label "Switch to workspace" :inputs
    	      ((:label "Workspace 1" :action :workspace-1)
    	       (:label "Workspace 2" :action :workspace-2)
    	       (:label "Workspace 3" :action :workspace-3)
    	       (:label "Workspace 4" :action :workspace-4)))
      (:label "Go back to the previous workspace" :action :previous-workspace)
      (:label "Create a new workspace" :action :create-workspace)
      (:label "Rename this workspace" :action :rename-workspace)
      (:label "Delete this workspace" :action :delete-workspace)
      (:label "Workspace settings" :action :configure-workspaces)))
    ;; (:label "Windows"
    ;;  :inputs
    ;;  ((:label "Create a new window" :action :create-window)
    ;;   (:label "Switch to the next window" :action :next-window)
    ;;   (:label "Switch to window" :action :switch-window)
    ;;   (:label "Close this window" :action :dismiss-window)))
    ;; (:label "Devices"
    ;;  :inputs
    ;;  ((:label "Browse available devices" :action :browse-devices)
    ;;   (:label "Scan for devices" :action :scan-devices)
    ;;   (:label "Configure joystick" :action :configure-joystick)
    ;;   (:label "Configure camera" :action :configure-camera)
    ;;   (:label "Configure microphone" :action :configure-microphone)
    ;;   (:label "Configure dance pad" :action :configure-dance-pad)))
    (:label "Help"
     :inputs
     ((:label "Copyright notice" :action :show-copyright-notice)
      (:label "General help" :action :general-help)
      (:label "Examples" :action :show-examples)
      (:label "Language Reference" :action :language-reference)))))

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

(define-block (window :super list)
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

;;; The system menu itself

(define-block-macro shell 
    (:super :list 
     :fields 
     ((category :initform :system))
     :inputs 
     (:headline (new 'headline)
      :menu 
      (new 'tree 
	   :label *xelf-title-string*
	   :pinned t
	   :category :system
	   :expanded t
	   :inputs
	   (list 
	    (new 'listener)
	    (new 'menu :label "Menu" 
		      :inputs 
		 (flet ((process (entry)
			  (make-menu entry :target self)))
		   (mapcar #'process *shell-menu-entries*))
		      :category :menu
		      :expanded t)
	    (new 'tree :label "Messages"
		 :expanded nil
		 :inputs (list (new 'messenger)))))))
  (unfreeze self)
  (expand %%menu)
  (mapc #'pin %inputs)
  (mapc #'pin (%inputs %%menu))
  (setf %locked nil))

(defun make-shell ()
  (find-uuid 
   (new '"XELF:SHELL")))

(define-method alternate-tap shell (x y))
(define-method scroll-tap shell (x y))

(define-method destroy shell ()
  (destroy%super self)
  (setf (%shell (world)) nil))

(define-method menu-items shell ()
  (%inputs %%menu))

(define-method get-listener shell ()
  (first (menu-items self)))
        
(define-method layout shell ()
  (layout-vertically self)
  ;; adjust header for project name
  (setf %width 
	(max %width
	     (+ *logo-height* (dash 5) 
		(font-text-width *project*
				 *block-bold*)))))

;; (define-method can-pick shell ()
;;   t)

;; (define-method pick shell ()
;;   self)

(define-method draw shell ()
  (with-fields (x y width height) self
    (draw-patch self x y (+ x width) (+ y height))
    (mapc #'draw %inputs)))

(define-method close-menus shell ()
  (let ((menus (menu-items self)))
    (when (some #'expandedp menus)
      (mapc #'unexpand menus))))

(define-method drop-dialog shell (dialog)
  (multiple-value-bind (x y) (right-of self)
    (add-block (world) dialog x y)))

;; Don't allow anything to be dropped on the menus, for now.

(define-method draw-hover shell () nil)

(define-method accept shell (thing)
  (declare (ignore thing))
  nil)

(define-method tap shell (x y)
  (declare (ignore x y))
  (grab-focus (get-listener self)))

;; ;;; Creating a project

;; (define-block-macro create-project-dialog 
;;     (:super :list
;;      :fields ((category :initform :system))
;;      :inputs (:name (new 'string :label "Project name:")
;; 	      :parent (new 'string :label "Create in folder:" 
;; 				   :value (namestring (projects-directory)))
;; 	      :folder-name (new 'string :label "Project folder name (optional):")
;; 	      :messenger (new 'messenger "Use the text entry fields above to name your new project.")
;; 	      :buttons (new 'hlist
;; 			    (new 'button :label "Create project"
;; 				 :target self :method :create-project)
;; 			    (new 'button :label "Dismiss"
;; 				 :target self :method :dismiss)))))

;; (define-method create-project create-project-dialog ()
;;   (with-input-values (name parent folder-name) self
;;     (add-message 
;;      %%messenger    
;;      (if (create-project-image 
;; 	  name :folder-name folder-name :parent parent)
;; 	 "Successfully created new project."
;; 	 "Could not create project."))))

;; (define-method create-project shell ()
;;   (let ((dialog (new 'window 
;; 		     :title "Create a new project"
;; 		     :child (new 'create-project-dialog))))
;;     (drop-dialog self dialog)))

;; ;;; Loading a project

;; (define-block-macro load-project-dialog 
;;     (:super :list
;;      :fields ((category :initform :system))
;;      :inputs (:filename 
;; 	      (new 'string :label "Load from folder:" 
;; 			   :value (namestring (projects-directory)))
;; 	      :messenger (new 'messenger "Use the text entry fields above to name your project.")
;; 	      :buttons (new 'hlist
;; 			    (new 'button :label "Load project"
;; 				 :target self :method :load-project)
;; 			    (new 'button :label "Dismiss"
;; 				 :target self :method :dismiss)))))

;; (define-method load-project load-project-dialog ()
;;   (with-input-values (filename) self
;;     (add-message 
;;      %%messenger    
;;      (if (load-project-image filename)
;; 	 "Successfully loaded new project."
;; 	 "Could not load project."))))

;; (define-method load-project shell ()
;;   (let ((dialog (new 'window 
;; 		     :title "Load a project"
;; 		     :child (new 'load-project-dialog))))
;;     (drop-dialog self dialog)))

;; ;;; Saving a project

;; (define-block-macro save-project-dialog 
;;     (:super :list
;;      :fields ((category :initform :system))
;;      :inputs (:messenger (new 'messenger "Save current project?")
;; 	      :buttons (new 'hlist
;; 			    (new 'button :label "Save project"
;; 				 :target self :method :save-project)
;; 			    (new 'button :label "Dismiss"
;; 				 :target self :method :dismiss)))))

;; (define-method save-project save-project-dialog ()
;;   (add-message 
;;    %%messenger    
;;    (if (save-project-image :force)
;;        "Successfully saved project."
;;        "Could not save project!")))

;; (define-method save-project shell ()
;;   (let ((dialog (new 'window 
;; 		     :title "Save current project"
;; 		     :child (new 'save-project-dialog))))
;;     (assert (windowp dialog))
;;     (drop-dialog self dialog)))

;;; shell.lisp ends here
