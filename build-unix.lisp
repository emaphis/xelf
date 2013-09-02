(require 'sb-posix)

(load #P"~/quicklisp/setup.lisp")
(push (merge-pathnames "lib/" *default-pathname-defaults*)
      asdf:*central-registry*)

(ql:quickload '(:lispbuilder-sdl-mixer :lispbuilder-sdl-ttf :lispbuilder-sdl-image :uuid :cl-opengl :cl-fad))
(asdf:load-system :xelf)

(push #p"/home/dto/2x0ng/" asdf:*central-registry*)

(asdf:oos 'asdf:load-op '2x0ng)
(sb-ext:save-lisp-and-die "2x0ng.bin"
			  :toplevel (lambda ()
				      (sb-posix:putenv
				       (format nil "SBCL_HOME=~A" 
				      	       #.(sb-ext:posix-getenv "SBCL_HOME")))
				      (setf xelf::*executable* t)
				      (setf xelf::*suppress-warnings* t)
				      (2x0ng:2x0ng)
				      0)
			  :executable t)

