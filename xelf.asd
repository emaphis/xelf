;;; -*- Mode: Lisp; -*-

;; ASDF Manual: http://constantly.at/lisp/asdf/

(defpackage :xelf-asd)

(in-package :xelf-asd)

(asdf:defsystem xelf
  :name "xelf"
  :version "2.01"
  :maintainer "David T O'Toole <dto@blocky.io>"
  :author "David T O'Toole <dto@blocky.io>"
  :license "General Public License (GPL) Version 3"
  :description "XELF is a visual programming language for Common Lisp."
  :serial t
  :depends-on (:lispbuilder-sdl 
	       :lispbuilder-sdl-image 
	       :lispbuilder-sdl-ttf
	       :lispbuilder-sdl-mixer
	       :uuid
;;	       :gettext
	       :cl-fad
	       :cl-opengl)
  :components ((:file "xelf")
	       (:file "rgb" :depends-on ("xelf"))
	       (:file "keys" :depends-on ("xelf"))
	       (:file "math" :depends-on ("xelf"))
	       (:file "logic" :depends-on ("xelf"))
	       (:file "prototypes" :depends-on ("xelf"))
	       (:file "quadtree" :depends-on ("xelf"))
	       (:file "console" :depends-on ("prototypes" "quadtree" "rgb" "keys" "math" "logic"))
	       (:file "blocks" :depends-on ("console"))
	       (:file "text" :depends-on ("blocks"))
	       (:file "phrase" :depends-on ("blocks"))
	       (:file "entry" :depends-on ("phrase" "text"))
	       (:file "words" :depends-on ("entry"))
	       (:file "halo" :depends-on ("phrase"))
	       (:file "sidebar" :depends-on ("entry"))
	       (:file "minibuffer" :depends-on ("sidebar"))
	       (:file "buffers" :depends-on ("entry" "text"))))

	       ;; (:file "syntax" :depends-on ("blocks"))))
;	       (:file "trees" :depends-on ("phrase"))
;	       (:file "program" :depends-on ("blocks"))
	       
