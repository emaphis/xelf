(require 'sb-posix)

(load "z:/home/dto/.wine/drive_c/users/dto/quicklisp/setup.lisp")
(push (merge-pathnames "lib/" *default-pathname-defaults*)
      asdf:*central-registry*)

(ql:quickload '(:lispbuilder-sdl-mixer :lispbuilder-sdl-ttf :lispbuilder-sdl-image :uuid :cl-opengl :cl-fad))
(asdf:load-system :xelf)

;; (push #p"/home/dto/2x0ng/" asdf:*central-registry*)

(asdf:oos 'asdf:load-op '2x0ng)
(sb-ext:save-lisp-and-die "2x0ng.exe"
			  :toplevel (lambda ()
				      (sb-posix:putenv
				       (format nil "SBCL_HOME=~A" 
				      	       #.(sb-ext:posix-getenv "SBCL_HOME")))
				      (setf xelf::*executable* t)
				      (setf xelf::*suppress-warnings* t)
				      (2x0ng:2x0ng)
				      0)
			  :executable t)



;; #!+win32   (!gencgc-space-setup #x22300000 nil nil #x10000)


;; export HOME=c:/users/dto
;; export PATH=$PATH:/mingw/msys/1.0/bin
;; export PATH=$PATH:/mingw/bin


;; C:\MinGW\   /mingw
;; H:\xelf\ /xelf
;; H:\2x0ng\ /2x0ng
;; C:\users\dto\quicklisp\ /quicklisp




;; #!+win32   (!gencgc-space-setup #x22100000 #x22300000 nil #x10000)


;; (def!constant gc-safepoint-page-addr #x21000000)

;; (def!constant read-only-space-start #x22000000)
;; (def!constant read-only-space-end #x220ff000)

;; (def!constant static-space-start #x22100000)
;; (def!constant static-space-end #x221ff000)

;; (def!constant dynamic-space-start #x22300000)
;; (def!constant dynamic-space-end (!configure-dynamic-space-end))

;; (def!constant linkage-table-space-start #x22200000)
;; (def!constant linkage-table-space-end #x222ff000)
