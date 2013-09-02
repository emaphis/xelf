;;; xelf.el --- Emacs tools for xelf

;; Copyright (C) 2006, 2007, 2008, 2009, 2010 David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: lisp, oop, extensions
;; Version: 0.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'rx)
(require 'cl)

(defun eval-in-cl (cl-expression-string &optional process-result-values)
  (slime-eval-async `(swank:eval-and-grab-output ,cl-expression-string)
    (lexical-let  ((here (current-buffer))
                   (process-result-values process-result-values))
      (lambda (result-values)
	(when process-result-values
	  (set-buffer here)
	  (funcall process-result-values (rest result-values)))))))

(defun xelf-insinuate-lisp ()
  (interactive)
  (add-hook 'lisp-mode-hook
	    #'(lambda ()
		(add-to-list 'imenu-generic-expression 
			     `("Methods" ,(rx (sequence "(" (group "define-method")
							(one-or-more space)
							(group (one-or-more (not (any space)))
							       (one-or-more space)
							       (one-or-more (not (any space))))))
					 2))
		(add-to-list 'imenu-generic-expression 
			     `("Blocks" ,(rx (sequence "(" (group "define-block")
						       (zero-or-more "-macro")
						       (one-or-more space)
						       (zero-or-one "(")
						       (group (one-or-more (or "-" (any word))))))
					2))
		(imenu-add-menubar-index)))
  (defadvice slime-compile-defun (after xelf activate)
    (eval-in-cl "(xelf:update-parameters)")))

;; (xelf-insinuate-lisp)

;;; Font-locking

;; Put this in your emacs initialization file to get the highlighting:
;; (add-hook 'emacs-lisp-mode-hook #'xelf-do-font-lock)

(defvar xelf-font-lock-keywords
  `((,(rx (sequence "(" (group "define-method")
		   (one-or-more space)
		   (group (one-or-more (not (any space))))
		   (one-or-more space)
		   (group (one-or-more (not (any space))))))
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face) ;; this still doesn't work
				      ;; properly.
     (3 font-lock-type-face))
    (,(rx (sequence "(" (group "define-prototype")
		   (one-or-more space)
		   (group (one-or-more (not (any space))))))
      (1 font-lock-keyword-face)
      (2 font-lock-type-face))
    (,(rx (sequence "(" (group "define-block-macro")
		   (one-or-more space)
		   (group (one-or-more (not (any space))))))
      (1 font-lock-keyword-face)
      (2 font-lock-type-face))
    (,(rx (sequence "(" (group "define-block")
		   (one-or-more space)
		   (group (one-or-more (not (any space))))))
      (1 font-lock-keyword-face)
      (2 font-lock-type-face))
;    ("\\<\\(\<[^<>]*\>\\)\\>" (1 font-lock-preprocessor-face))
    ("(.*\\(\>\>\\>\\)" (1 font-lock-type-face))))

(defun xelf-do-font-lock ()
  (interactive)
  "Highlight the keywords used in prototype-oriented programming."
  (font-lock-add-keywords nil xelf-font-lock-keywords))

;;; Grabbing UUIDs and inspecting the corresponding objects

(defvar xelf-uuid-regexp 
  "[0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F]")

(defun xelf-inspect-uuid (uuid)
  (interactive "sInspect xelf UUID: ")
  (if (null uuid)
      (message "No UUID provided.")
      (progn 
	(assert (stringp uuid))
	(slime-inspect
	 (format "(xelf::find-object %S)" uuid)))))

(defun xelf-uuid-at-point ()
  (let ((thing (thing-at-point 'word)))
    (when (and (not (null thing))
	       (string-match xelf-uuid-regexp thing))
      thing)))
	  
(defun xelf-uuid-on-this-line ()
  (string-match xelf-uuid-regexp
		(buffer-substring-no-properties
		 (point-at-bol)
		 (point-at-eol))))

(defun xelf-inspect ()
  (interactive)
  (xelf-inspect-uuid (or (xelf-uuid-at-point)
			   (xelf-uuid-on-this-line))))

  ;; (set-frame-parameter nil 'background-mode 'dark)
  ;; (set-frame-parameter nil 'background-color "gray40")
  ;; (set-frame-parameter nil 'foreground-color "white")
  ;; (set-frame-parameter nil 'border-color "gray20")
  ;; (set-frame-parameter nil 'cursor-color "magenta"))

(provide 'xelf)
;;; xelf.el ends here
