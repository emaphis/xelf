;;; commands.lisp --- user level interactive commands for Xelf editor

;; Copyright (C) 2013  David O'Toole

;; Author: David O'Toole <dto@xiomacs>
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

(define-command save-buffer-as ((filename (buffer-file-name (current-buffer))))
  (message "~S filename" filename))
  
(define-command resize-buffer ((width (%width (current-buffer)))
			       (height (%height (current-buffer))))
  (message "RESIZE ~S ~S" width height)
  (resize (current-buffer) width height))

;;; commands.lisp ends here
