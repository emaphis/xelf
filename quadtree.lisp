;;; quadtree.lisp --- for spatial indexing and stuff

;; Copyright (C) 2011, 2012  David O'Toole

;; Author: David O'Toole <dto@blocky.io>
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

;;; Code:

(in-package :xelf)

(defvar *cached-quadtree* nil)

(defvar *quadtree* nil)

(defvar *buffer* nil
"The current buffer object. Only one may be active at a time. See also
buffers.lisp. Sprites and cells are free to send messages to `*buffer*'
at any time, because `*buffer*' is always bound to the buffer containing
the object when the method is run.")

(defmacro with-quadtree (quadtree &rest body)
  `(let* ((*quadtree* ,quadtree))
       ,@body))

(defvar *quadtree-depth* 0)

(defparameter *default-quadtree-depth* 11) 
 
(defstruct quadtree 
  objects level
  top left right bottom
  southwest northeast northwest southeast)

(defmethod print-object ((tree xelf::quadtree) stream)
  (format stream "#<XELF:QUADTREE count: ~S>"
	  (length (quadtree-objects tree))))

(defun leafp (node)
  ;; testing any quadrant will suffice
  (null (quadtree-southwest node)))

(defun bounding-box-contains (box0 box1)
  (destructuring-bind (top0 left0 right0 bottom0) box0
    (destructuring-bind (top1 left1 right1 bottom1) box1
      (and (<= top0 top1)
	   (<= left0 left1)
	   (>= right0 right1)
	   (>= bottom0 bottom1)))))

(defun quadtree-contains (quadtree top left right bottom)
  (declare (single-float top left right bottom))
  (and (<= (the single-float (quadtree-top quadtree)) top)
       (<= (the single-float (quadtree-left quadtree)) left)
       (>= (the single-float (quadtree-right quadtree)) right)
       (>= (the single-float (quadtree-bottom quadtree)) bottom)))

(defun scale-bounding-box (box factor)
  (destructuring-bind (top left right bottom) box
    (let ((margin-x (* (- right left)
		       (- factor 1.0)))
	  (margin-y (* (- bottom top)
		       (- factor 1.0))))
      (values (- top margin-y)
	      (- left margin-x)
	      (+ right margin-x)
	      (+ bottom margin-y)))))

(defun valid-bounding-box (box)
  (and (listp box)
       (= 4 (length box))
       (destructuring-bind (top left right bottom) box
	 (and (<= left right) (<= top bottom)))))

(defun northeast-quadrant (bounding-box)
;  (assert (valid-bounding-box bounding-box))
  (destructuring-bind (top left right bottom) bounding-box
    (list top (cfloat (/ (+ left right) 2))
	  right (cfloat (/ (+ top bottom) 2)))))

(defun southeast-quadrant (bounding-box)
;  (assert (valid-bounding-box bounding-box))
  (destructuring-bind (top left right bottom) bounding-box
    (list (cfloat (/ (+ top bottom) 2)) (cfloat (/ (+ left right) 2))
	  right bottom)))

(defun northwest-quadrant (bounding-box)
;  (assert (valid-bounding-box bounding-box))
  (destructuring-bind (top left right bottom) bounding-box
    (list top left
	  (cfloat (/ (+ left right) 2)) (cfloat (/ (+ top bottom) 2)))))

(defun southwest-quadrant (bounding-box)
;  (assert (valid-bounding-box bounding-box))
  (destructuring-bind (top left right bottom) bounding-box
    (list (cfloat (/ (+ top bottom) 2)) left
	  (cfloat (/ (+ left right) 2)) bottom)))

(defun quadtree-process (top left right bottom processor &optional (node *quadtree*))
  (when (quadtree-contains node top left right bottom)
    (when (not (leafp node))
      (let ((*quadtree-depth* (1+ *quadtree-depth*)))
	(quadtree-process top left right bottom processor (quadtree-northwest node))
	(quadtree-process top left right bottom processor (quadtree-northeast node))
	(quadtree-process top left right bottom processor (quadtree-southwest node))
	(quadtree-process top left right bottom processor (quadtree-southeast node))))
    (funcall processor node)))

(defun build-quadtree (bounding-box0 &optional (depth *default-quadtree-depth*))
  ;; (assert (plusp depth))
  ;; (assert (valid-bounding-box bounding-box0))
  (let ((bounding-box (mapcar #'cfloat bounding-box0)))
    (destructuring-bind (top left right bottom) bounding-box
      (decf depth)
      (if (zerop depth)
	  (make-quadtree :top top :left left :right right :bottom bottom)
	  (make-quadtree :top top :left left :right right :bottom bottom
			 :northwest (build-quadtree (northwest-quadrant bounding-box) depth)
			 :northeast (build-quadtree (northeast-quadrant bounding-box) depth)
			 :southwest (build-quadtree (southwest-quadrant bounding-box) depth)
			 :southeast (build-quadtree (southeast-quadrant bounding-box) depth))))))

(defun rebuild-node (node top left right bottom)
  (when node
    (prog1 node
      (setf (quadtree-top node) (cfloat top))
      (setf (quadtree-left node) (cfloat left))
      (setf (quadtree-right node) (cfloat right))
      (setf (quadtree-bottom node) (cfloat bottom))
      (setf (quadtree-objects node) nil)
      (rebuild-node (quadtree-northeast node)
		    top (cfloat (/ (+ left right) 2))
		    right (cfloat (/ (+ top bottom) 2)))
      (rebuild-node (quadtree-southeast node)
		    (cfloat (/ (+ top bottom) 2)) (cfloat (/ (+ left right) 2))
		    right bottom)
      (rebuild-node (quadtree-northwest node)
		    top left
		    (cfloat (/ (+ left right) 2)) (cfloat (/ (+ top bottom) 2)))
      (rebuild-node (quadtree-southwest node)
		    (cfloat (/ (+ top bottom) 2)) left
		    (cfloat (/ (+ left right) 2)) bottom))))

(defun rebuild-quadtree (bounding-box &optional depth)
  (destructuring-bind (top left right bottom) bounding-box
    (setf *cached-quadtree*
	  (rebuild-node (or *cached-quadtree* 
			    (build-quadtree bounding-box depth))
			top left right bottom))))

(defun quadtree-search (top left right bottom &optional (node *quadtree*))
  "Return the smallest quadrant enclosing TOP LEFT RIGHT BOTTOM at or below
NODE, if any."
  ;; (assert (quadtree-p node))
  ;; (assert (valid-top left right bottom top left right bottom))
  ;; (message "~A ~A Searching quadrant ~S for bounding box ~S" 
  ;; 	   *quadtree-depth* (make-string (1+ *quadtree-depth*) :initial-element (character "."))
  ;; 	   (quadtree-top left right bottom node) top left right bottom)
  (when (quadtree-contains node top left right bottom)
    ;; ok, it's in the overall bounding-box.
    (if (leafp node)
	;; there aren't any quadrants to search. stop here.
	node
	(or
	 ;; search the quadrants.
	 (let ((*quadtree-depth* (1+ *quadtree-depth*)))
	   (or (quadtree-search top left right bottom (quadtree-northwest node))
	       (quadtree-search top left right bottom (quadtree-northeast node))
	       (quadtree-search top left right bottom (quadtree-southwest node))
	       (quadtree-search top left right bottom (quadtree-southeast node))))
	 ;; none of them are suitable. stay here
	 node))))

(defun quadtree-insert (object &optional (tree *quadtree*))
  (let ((node0 
	  (multiple-value-bind (top left right bottom) (bounding-box object)
	    (quadtree-search top left right bottom tree))))
    (let ((node (or node0 tree)))
      ;; (message "Inserting ~S ~S"
      ;; 	       (get-some-object-name object) 
      ;; 	       (object-address-string object))
      ;; (assert (not (find (find-object object)
      ;; 			 (quadtree-objects node)
      ;; 			 :test 'eq)))
      (pushnew (find-object object)
	       (quadtree-objects node)
	       :test 'eq)
      ;; save pointer to node so we can avoid searching when it's time
      ;; to delete (i.e. move) the object later.
      (xelf:set-field-value :quadtree-node object node))))
      ;; (assert (find (find-object object)
      ;; 		    (quadtree-objects node)
      ;; 		    :test 'eq)))))

(defun quadtree-delete (object0 &optional (tree *quadtree*))
  (let ((object (find-object object0)))
    ;; grab the cached quadtree node
    (let ((node (or (field-value :quadtree-node object) tree)))
      ;; (assert node)
      ;; (assert (find object
      ;; 		    (quadtree-objects node)
      ;; 		    :test 'eq))
      (setf (quadtree-objects node)
	    (delete object (quadtree-objects node) :test 'eq))
      (set-field-value :quadtree-node object nil))))
      ;; (assert (not (find object
      ;; 			 (quadtree-objects node)
      ;; 			 :test 'eq))))))

(defun quadtree-insert-maybe (object &optional (tree *quadtree*))
  (when tree
    (quadtree-insert object tree)))

(defun quadtree-delete-maybe (object &optional (tree *quadtree*))
  (when (and tree (field-value :quadtree-node object))
    (quadtree-delete object tree)))

(defun quadtree-map-collisions (top left right bottom processor &optional (tree *quadtree*))
  ;; (assert (functionp processor))
  ;; (assert (valid-bounding-box bounding-box))
  (quadtree-process
   top left right bottom
   #'(lambda (node)
       (dolist (object (quadtree-objects node))
	 (when (colliding-with-bounding-box object top left right bottom)
	   (funcall processor object))))
   tree))

(defun quadtree-collide (object &optional (tree *quadtree*))
  (multiple-value-bind (top left right bottom) (bounding-box object)
    (quadtree-map-collisions 
     top left right bottom
     #'(lambda (thing)
	 (when (and (xelfp thing) 
		    (xelfp object)
		    (field-value :collision-type thing)
		    (colliding-with object thing)
		    (not (object-eq object thing)))
	   (with-quadtree tree
	     (collide object thing))))
     tree)))

(defun find-bounding-box (objects)
  ;; calculate the bounding box of a list of objects
  ;; (assert (not (null objects)))
  (labels ((left (thing) (field-value :x thing))
	   (right (thing) (+ (field-value :x thing)
			     (field-value :width thing)))
	   (top (thing) (field-value :y thing))
	   (bottom (thing) (+ (field-value :y thing)
			      (field-value :height thing))))
    ;; let's find the bounding box.
    (values (reduce #'min (mapcar #'top objects))
	    (reduce #'min (mapcar #'left objects))
	    (reduce #'max (mapcar #'right objects))
	    (reduce #'max (mapcar #'bottom objects)))))

(defun quadtree-fill (set &optional (quadtree *quadtree*))
  (let ((objects (etypecase set
		   (list set)
		   (hash-table (loop for object being the hash-keys in set collect object)))))
    (dolist (object objects)
;      (message "Filling ~S into quadtree" object)
      (set-field-value :quatree-node object nil)
      (quadtree-insert object quadtree))))

;;; quadtree.lisp ends here
