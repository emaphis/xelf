;;; xelf.lisp --- a visual dialect of Common Lisp inspired by Squeak
               
;; Copyright (C) 2006-2013 David O'Toole

;; Author: David O'Toole <dto@blocky.io>
;; Keywords: multimedia, games

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details. 

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; This program is dedicated to our beloved Yogi, who died 2006-10-06.
;; Re-dedicated June 2012 to our beloved Cookie-Puss.

;;; Requirements:

;; See the included file INSTALL.

;;; Code:

(defpackage :xelf
    (:documentation "A 2d game engine in Common Lisp.")
  (:use :common-lisp) 
  (:export null-block *frequency* *output-chunksize* *output-channels*
cfloat halt-sample *dt* *copyright-notice* *author*
*message-hook-functions* exit-xelf add-to-list shell-open-p z-sort
modify-joystick-profile defproject start stop selection cut copy paste
paste-here clear-clipboard copy-to-clipboard paste-at-pointer
at-next-update *next-update-hook* eval-in-emacs autoload
*already-serialized* browse back selected-object bounding-box-contains
*preload-resources* preload-resources *always-show-sidebar*
*preload-images* *preload-samples* prototype-variable-name
*update-function* kill-buffer *target* toggle-play *blocks* shut-down
later later-at toggle-glass glass-show glass-hide glass-show-at
later-when start-up seconds->frames keyboard-held-p keyboard-pressed-p
holding-control transform-window *scale-output-to-window*
keyboard-released-p *edit* with-font *font* find-heading
keyboard-time-in-current-state *current-directory* current-directory
pretty-string ugly-symbol *pointer-x* *pointer-y* is-joystick-event
*self* is-raw-joystick-event keyboard-time-in-previous-state *updates*
keyboard-down-p *buffers* keyboard-keys-down keyboard-modifier-down-p
find-buffer find-buffer *socket-size* keyboard-modifiers
draw-filled-circle draw-aa-circle get-keys make-block add-buffer
find-buffer add-block remove-block with-quadtree *initialization-hook*
hit-blocks quadtree-delete add-resources quadtree-insert
build-quadtree quadtree-collide quadtree-show *quadtree*
*quadtree-depth* split-string-on-lines message with-session
start-session *prompt-sweden-keybindings* *prompt-qwerty-keybindings*
*screen-width* transform-method-body roll-under initialize-colors
*style* load-project-image create-project-image *standard-categories*
*left-turn* bind-event *right-turn* left-turn right-turn roll
bind-event-to-method *colors* enable-key-repeat disable-key-repeat
get-color define-method *font* field-value set-field-value
object-fields dispatch-event *user-init-file-name* distance
icon-resource icon-image *directions* *opposites* *pending-resources*
with-project project-orthographically project-with-perspective
open-viewport find-resource-property compose-blank-fields font-width
font-height find-object *windows* edit create xelf
transform-field-reference define-block *screen-height*
formatted-line-width find-buffer *clipboard* formatted-line-height
formatted-string-height formatted-string-width get-color create-image
draw-image xelf edit define-prototype has-field *target* with-target
define-buffer set-field-options *user-joystick-profile* paste-into
*garbage-buffers* field-option-value index-resource index-all-images
index-all-samples *default-joystick-profile* joystick-profile visit
draw-textured-rectangle-* find-project-path index-project image-height
image-width load-image-resource load-lisp-resource *executable*
*combine-buffers-destructively* *screen-height* cursor *screen-width*
xelfp *nominal-screen-width* *nominal-screen-height*
*gl-screen-width* *gl-screen-height* *message-function* dash
holding-shift get-button-index message-to-standard-output
*cached-quadtree* rebuild-quadtree rebuild-node reset-message-function
*make-prototype-id-package* lturn rturn ticks-per-beat radian-angle
draw-textured-rectangle default-project-directories
*resource-handlers* load-resource find-resource find-resource-object
*colors* *buffer* make-directory-maybe load-user-init-file
*project-directories* resource-to-plist *osx* *linux* make-resource
make-object-resource make-event *blocks* bind-event-to-text-insertion
*user-projects-directory* make-field-initializer clone
number-of-joysticks make-field-initializer-body
make-key-modifier-symbol make-key-string normalize-event make-keyword
make-object queue-head queue-max queue-count *sender*
field-reference-p null-next object-eq *x11-color-data* object-name
object-parent send send-super send-queue self opposite-direction
opposite-heading object-address-string object step-in-direction
define-resource direction-to plasma-rect subdivide-rect render-plasma
add-hook run-hook queue-tail make-resource-link save-resource
on-screen-p save-project-image *system* *defined-resources*
save-project save-everything with-input-values with-inputs select-all
*export-formats* clear-selection export-archive *use-texture-blending*
defresource export-application paste-as-new-buffer
*default-texture-filter* export-project make-queue queue unqueue
*font-texture-filter* queue-message queued-messages-p unqueue-message
send-queue field-value random-direction random-choose *resources*
load-font-resource save-object-resource initialize%super
draw-string-solid read-box initialize-resource-table percent-of-time
render-formatted-paragraph make-formatted-string draw-string-shaded
set-blending-mode render-formatted-string render-formatted-line
resource font-text-width write-sexp-to-file with-message-sender
*message-sender* read-sexp-from-file with-fields with-field-values
write-blx *grammar* one-of left-hand-side right-hand-side expansions
generate send-event-to-blocks play-music halt-music seek-music
play-project *joystick-mapping* play initialize-sound
*generic-joystick-mapping* *joystick-button-symbols*
draw-resource-image *event-handler-function* *use-sound* midpoint
send-event self get-some-object-name
transform-declaration-field-descriptor no-such-field
find-projects-in-directory goal directory-is-project-p
find-directories find-all-projects *project* transform-tree
*after-startup-hook* draw-line operation-symbol message-symbol
play-sample set-music-volume draw-pixel *user-keyboard-layout*
*fullscreen* draw-circle set-field-option-value load-project
field-options current-buffer *frame-rate* set-resource-system-p
next-method *blx-file-extension* *project* *project-path*
*window-title* *window-position* restartably *message-logging* shell
window-pointer-x window-pointer-y use-filter set-blending-mode pushf
popf define-word forget-word forget-all-words execute-word execute
execute-string update-parameters joystick-axis-pressed-p
joystick-axis-value joystick-axis-raw-value analog-stick-heading
find-heading analog-stick-pressure evalf forth *joystick-axis-size*
*joystick-axis-dead-zone* *event-hook* left-analog-stick-heading
left-analog-stick-pressure *message-history* index-pending-resources
right-analog-stick-heading joystick-button-pressed-p
analog-stick-pressed-p left-analog-stick-pressed-p
right-analog-stick-pressed-p right-analog-stick-pressure
initialize-console joystick-axis-value poll-joystick-button
joystick-button-state reset-joystick *device-profiles*
*joystick-device* *joystick-device-number* button-to-symbol
symbol-to-button find-device-profile set-screen-width *play-args*
set-screen-height genseq save-objects enable-timer save-buffer
load-buffer draw-box *resizable* *resize-hook* draw-rectangle cursorp
switch-to-buffer *after-load-project-hook* *mission* mission-variable
find-bounding-box combine stack-vertically set-mission-variable
horizontal-extent vertical-extent flip-horizontally flip-vertically
mirror-horizontally mirror-vertically buffer with-mission-locals
with-empty-buffer define-turtle stack-horizontally *background-color*
combine-beside combine-below get-ticks *block-font* quit reset
seek-music make-keyword object field-value make-queue find-parent
set-field-value find-super *font* set-field-options field-options
field-documentation set-field-option-value field-option-value
*lookup-failure* no-such-field has-field has-method send send-queue
send-super serialize deserialize initialize-method-cache
*send-super-depth* initialize-documentation-tables null-parent queue
unqueue empty-queue *message-queue* queue-message make-non-keyword
with-fields queue-count queue-head method-documentation
set-method-documentation method-arglist method-arglist-for-swank
set-method-arglist queued-messages-p with-field-values with-fields-ex
unqueue-message unqueue-and-send-message with-message-queue
message-symbol operation-symbol *sender* message-reader transform-tree
field-reference-p transform-field-reference transform-method-body
object-parent object-name object-fields define-method
*joystick-dead-zone* define-prototype new object-p self percent-gray
percent-grey *indicators* find-indicator-texture draw-indicator
font-text-width transform-declaration-field-descriptor is-a
compose-blank-fields make-field-initializer initialize pause play
*image-opacity* rewind stop initialize-prototypes initialize-xelf
load-project update-future object-address-string draw-string make-tree
draw-string-blended make-menu find-text-image make-text-image
find-texture *default-super* clear-text-image-cache *token-types* text
direction-heading direction-degrees verify *serif*
*use-antialiased-text* *sans* *monospace* toggle-debug
*debug-on-error* *block-categories* *block-colors* input paste
arrange-beside arrange-below load-variable-resource translate
save-variable-resource window-x window-y *persistent-variables*
with-new-buffer with-border with-blank-buffer with-buffer-prototype
with-buffer remove-trailing-space *buffer-prototype* step-coordinates
*default-frame-rate* make-field-accessor-forms save-excursion
make-input-accessor-forms *persistent-variables-file-name* duplicate
persistent-variables-file combine save-variables indicator-size
draw-indicator load-variables *block-text-colors* defblock capture
paste-from make-input-accessor-macrolet-clause heading-direction
make-input-accessor-defun-forms input-reference-p input-block
*default-quadtree-depth* input-value *block-bold* *bold* *italic*
*block-italic* colliding-with-bounding-box define-block-macro heading-degrees))

