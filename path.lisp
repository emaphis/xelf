;;; path.lisp --- A* pathfinding for XELF

;; Copyright (C) 2009, 2011, 2012  David O'Toole

;; Author: David O'Toole %dto@blocky.io
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
;; along with this program.  If not, see %http://www.gnu.org/licenses/.

;;; Commentary:

;; What follows is an implementation of the well-known A* pathfinding
;; algorithm on a rectangular grid.

;; http://en.wikipedia.org/wiki/A-star_search_algorithm

;;; Code:

(in-package :xelf)

(defstruct path
  finder ;; Who is finding this path? 
  buffer ;; Pointer to associated buffer. 
  grid ;; Array of pathfinding data nodes.
  height width
  heap ;; Heap array of open pathfinding nodes.
  end ;; Pointer to last heap array position.
  turn ;; Integer turn number
  )

(defparameter *path-grid-resolution* 256)

(defun row-to-y (path row) 
  (let ((cy (/ (%height (path-buffer path))
	       (path-height path))))
    (cfloat (* cy row))))

(defun column-to-x (path column) 
  (let ((cx (/ (%width (path-buffer path))
	       (path-width path))))
    (cfloat (* cx column))))

(defun obstructed (path row column)
  (with-field-values (height width) 
      (path-buffer path)
    (let ((*quadtree* (%quadtree (path-buffer path)))
	  (border 8))
      (multiple-value-bind (top left right bottom)
	  (bounding-box (path-finder path))
	(let* ((utop (row-to-y path row))
	       (uleft (column-to-x path column))
	       (vtop (- utop border))
	       (vleft (- uleft border))
	       (vright (+ border vleft (- right left)))
	       (vbottom (+ border vtop (- bottom top))))
;;	  (message "VTOP ~S ~S ~S ~S" vtop vleft vright vbottom) 
	  (block colliding
	    (flet ((check (object)
		     (when (and (xelfp object)
				(field-value :collision-type object)
				(not (object-eq object (path-finder path)))
				(has-tag object :solid))
		       (return-from colliding object))))
	      (prog1 nil
		(quadtree-map-collisions 
		 (cfloat vtop)
		 (cfloat vleft)
		 (cfloat vright)
		 (cfloat vbottom) #'check)))))))))

(defstruct node 
  row 
  column
  parent ; previous node along generated path
  F ; node score, equal to G + H
  G ; movement cost to move from starting point
    ; to (row, column) along generated path
  old-G ; previous value of G
  H ; heuristic cost to reach goal from (row, column)
  closed ; equal to path's path-turn-number when on closed list
  open ; equal to path's path-turn-number when on open list
  )

(defun create-path (finder &key
			 (height *path-grid-resolution*)
			 (width *path-grid-resolution*) 
			 (buffer (current-buffer)))
  (assert (xelfp buffer))
  (let ((path (make-path :buffer buffer
			 :finder finder
			 :grid (make-array (list height width))
			 :heap (make-array (* height width))
			 :height height
			 :width width
			 :turn 1 :end 0)))
    (prog1 path
      (dotimes (r height)
	(dotimes (c width)
	  (setf (aref (path-grid path) r c)
		(make-node :row r :column c)))))))
			       
;; The following routines maintain the open and closed sets. We
;; use a minheap to store the open set.

(defun open-node (path node)
  (let* ((path-heap-end (if (null (path-end path))
			    (setf (path-end path) 1)
			    (incf (path-end path))))
	 (path-heap (path-heap path))
 	 (ptr path-heap-end)
	 (parent nil)
	 (finished nil))
    ;; make it easy to check whether node is open
    (setf (node-open node) (path-turn path))
    ;; add node to end of heap 
    (setf (aref path-heap path-heap-end) node)
    ;; let node rise to appropriate place in heap
    (while (and (not finished) (< 1 ptr))
      (setf parent (truncate (/ ptr 2)))
      ;; should it rise? 
      (if (< (node-F node) (node-F (aref path-heap parent)))
	  ;; yes. swap parent and node
	  (progn 
	    (setf (aref path-heap ptr) (aref path-heap parent))
	    (setf ptr parent))
	  ;; no. we're done.
	  (progn (setf finished t)
		 (setf (aref path-heap ptr) node))))
    ;; do we need to set node as the new root? 
    (if (and (not finished) (equal 1 ptr))
	(setf (aref path-heap 1) node))))

(defun close-node (path)
  (let* ((path-heap (path-heap path))
	 ;; save root of heap to return to caller
	 (node (aref path-heap 1))
	 (last nil)
	 (path-heap-end (path-end path))
	 (ptr 1)
	 (left 2)
	 (right 3)
	 (finished nil))
    ;; is there only one node?
    (if (equal 1 path-heap-end)
	(setf (path-end path) nil)
      (if (null path-heap-end)
	  nil
	;; remove last node of heap and install as root of heap
	 (progn
	   (setf last (aref path-heap path-heap-end))
	   (setf (aref path-heap 1) last)
	   ;; shrink heap
	   (decf (path-end path))
	   (decf path-heap-end)
	   ;;
	   (setf (node-closed node) (path-turn path))
	   ;;
	   ;; figure out where former last element should go
	   ;;
	   (while (and (not finished) (>= path-heap-end right))
	     ;;
	     ;; does it need to sink? 
	     (if (and (< (node-F last) (node-F (aref path-heap left)))
		      (< (node-F last) (node-F (aref path-heap right))))
		 ;;
		 ;; no. we're done
		 (progn 
		   (setf finished t)
		   (setf (aref path-heap ptr) last))
		 ;;
		 ;; does it need to sink rightward?
		 (if (>= (node-F (aref path-heap left)) 
			 (node-F (aref path-heap right)))
		     ;;
		     ;; yes
		     (progn
		       (setf (aref path-heap ptr) (aref path-heap right))
		       (setf ptr right))
		     ;;
		     ;; no, sink leftward
		     (progn
		       (setf (aref path-heap ptr) (aref path-heap left))
		       (setf ptr left))))
	     (setf left (* 2 ptr))
	     (setf right (+ 1 left)))
	   ;;
	   ;; 
	   (if (and (equal left path-heap-end)
		    (> (node-F last)
		       (node-F (aref path-heap left))))
	       (setf ptr left)))))
	;;
	;; save former last element in its new place
	(setf (aref path-heap ptr) last)
    node))

;; The ordinary distance algorithm is used to score nodes.

(defun score-node (path node path-turn-number new-parent-node goal-row goal-column)
  "Update scores for NODE. Update heap position if necessary."
  (let* ((direction (direction-to (node-column new-parent-node)
				  (node-row new-parent-node)
				  (node-column node)
				  (node-row node)))
	 (G (+ 1 (node-G new-parent-node)))
	 
 	 (H (* (distance (node-column node)
			 (node-row node)
			 goal-column goal-row)
		;; (max (abs (- (node-row node) goal-row))
		;;     (abs (- (node-column node) goal-column)))
	       1))
	 (F (+ G H)))
    ;; 
    ;; is this a new node, i.e. not on the open list? 
    (if (not (equal path-turn-number (node-open node)))
	;;
	;; yes, update its scores and parent
	(progn 
	  (setf (node-G node) G)
	  (setf (node-H node) H)
	  (setf (node-F node) F)
	  (setf (node-parent node) new-parent-node))
      ;;
      ;; no, it's already open. is the path through NEW-PARENT-NODE
      ;; better than through the old parent?
      (if (and (node-G node)
	       (< G (node-G node)))
	  ;;
	  ;; yes. update scores and re-heap.
	  (let ((heap (path-heap path))
		(heap-end (path-end path))
		(ptr 1)
		(par nil)
		(finished nil))
	    (setf (node-G node) G)
	    (setf (node-H node) H)
	    (setf (node-F node) F)
	    (setf (node-parent node) new-parent-node)
	    ;;
	    ;; Better score found.
	    ;; 
	    ;; find current location of node in heap
	    (while (and (not finished) (< ptr heap-end))
	      (when (equal node (aref heap ptr))
		;; Found node.
		;;
		;; its score could only go down, so move it up in the
		;; heap if necessary.
		(while (and (not finished) (< 1 ptr))
		  (setf par (truncate (/ ptr 2)))
		  ;;
		  ;; should it rise? 
		  (if (< (node-F node) (node-F (aref heap par)))
		      ;;
		      ;; yes. swap it with its parent
		      (progn
			(setf (aref heap ptr) (aref heap par))
			(setf ptr par))
		    ;;
		    ;; no, we are done. put node in its new place.
		      (progn (setf finished t)
			     (setf (aref heap ptr) node))))
		;;
		;; do we need to install the new node as heap root?
		(when (and (not finished) (equal 1 ptr))
		  (setf (aref heap 1) node)))
	      ;;
	      ;; keep scanning heap for the node
	      (incf ptr)))
	;;
	;; new score is not better. do nothing.
					;(setf (node-parent node) new-parent-node)
	  ))))
	      
(defun node-successors (path node path-turn-number goal-row goal-column)
  (delete nil 
	(mapcar 
	 #'(lambda (direction)
	     (let ((grid (path-grid path))
		   (new-G (+ 1 (node-G node)))
		   (successor nil))
	       (multiple-value-bind (r c) 
		   (step-in-direction 
		    (node-row node)
		    (node-column node)
		    direction)
		 ;; 
		 (if (array-in-bounds-p grid r c)
		     (progn 
		       (setf successor (aref grid r c))
		       
		       (if (or 
			    ;; always allow the goal square even when it's an obstacle.
			    (and (equal r goal-row) (equal c goal-column))
			    ;; ignore non-walkable squares and closed squares,
			    (and (not (obstructed path r c))
				 (not (equal path-turn-number (node-closed successor)))))
			   ;; if successor is open and existing path is better
			   ;; or as good as new path, destroy the successor
			   ;; if successor is not open, proceed 
			   (if (equal path-turn-number (node-open successor))
			       (if (< new-G (node-G successor))
				   successor
				   nil)
			       successor)
			   nil))
		     nil))))
	 *directions*)))
  
  ;; Now we come to the pathfinding algorithm itself. 
  
(defun find-path (path x0 y0 x1 y1)
  "Find a path from the starting point to the goal in PATH using A*.
Returns a list of directional keywords an AI can follow to reach
the goal."
  (let* ((selected-node nil)
	 (path-turn-number (incf (path-turn path)))
	 (pos nil)
	 (found nil)
	 (path-height (path-height path))
	 (path-width (path-width path))
	 (buffer-height (%height (path-buffer path)))
	 (buffer-width (%width (path-buffer path)))
	 (cx (/ buffer-width path-width))
	 (cy (/ buffer-height path-height))
	 (target-node nil)
	 (coordinates nil)
	 (F 0) (G 0) (H 0)
	 (starting-row (round (/ y0 cy)))
	 (starting-column (round (/ x0 cx)))
	 (goal-row (round (/ y1 cy)))
	 (goal-column (round (/ x1 cx))))
    ;; reset the pathfinding heap
    (setf (path-end path) nil)
    ;; add the starting node to the open set
    (setf G 0)
    (setf H (max (abs (- starting-row goal-row))
		 (abs (- starting-column goal-column))))
    (setf F (+ G H))
    (setf selected-node (make-node :row starting-row 
				       :column starting-column
				       :old-G 0
				       :parent nil :G G :F F :H H))
    ;;
    (open-node path selected-node)
    ;; start pathfinding
    (setf target-node
	  (block finding
	    ;; select and close the node with smallest F score
	    (while (setf selected-node (close-node path))
	      ;; did we fail to reach the goal? 
	      (when (null selected-node)
		(return-from finding nil))
	      ;; are we at the goal square?
	      (when (and (equal goal-row (node-row selected-node))
			 (equal goal-column (node-column selected-node)))
		(return-from finding selected-node))
	      ;; process adjacent walkable non-closed nodes
	      (mapc #'(lambda (node)
			;; is this cell already on the open list?
			(if (equal path-turn-number (node-open node))
			    ;; yes. update scores if needed
			    (score-node path node path-turn-number
					selected-node goal-row goal-column)
			    (progn 
			      ;; it's not on the open list. add it to the open list
			      (score-node path node path-turn-number selected-node
					  goal-row goal-column)
			      (open-node path node))))
		    ;; map over adjacent nodes
		    (node-successors path selected-node 
				     path-turn-number
				     goal-row goal-column)))))
    ;; did we find a path? 
    (if (node-p target-node)
	;; save the path by walking backwards from the target
	(let ((previous-node target-node)
	      (current-node nil))
	  (while (setf current-node (node-parent previous-node))
	    ;; what direction do we travel to get from current to previous? 
	    (push (list (node-row current-node)
			(node-column current-node))
		  coordinates)
	    (setf previous-node current-node))
	  ;; return the finished path
	  coordinates)
	;; return nil
	nil)))

(defun address-to-waypoint (path address)
  (destructuring-bind (row column) address
    (list (round (column-to-x path column))
	  (round (row-to-y path row)))))

(defun find-path-waypoints (path x0 y0 x1 y1)
  (mapcar #'(lambda (address)
	      (address-to-waypoint path address))
	  (find-path path (truncate x0) 
		     (truncate y0)
		     (truncate x1)
		     (truncate y1))))

(defun print-path (foo stream)
  (format stream "#<% XELF PATH>"))

(defmethod print-object ((foo xelf::path) stream)
  (print-path foo stream))

;;; path.lisp ends here
