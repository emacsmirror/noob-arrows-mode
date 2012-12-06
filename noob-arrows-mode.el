;;; noob-arrows-mode.el --- Noob arrows minor mode

;; Copyright 2012, Nick Van Horn

;; Author: Nick Van Horn <nemo1211@gmail.com>
;; Keywords: noob, arrows, novice, help, training
;; Version: 1.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:

;; Activating noob-arrows-mode or global-noob-arrows-mode turns the
;; arrow keys into help buttons. This forces the novice Emacs user to
;; use alternative navigation techniques (not that this is necessarily
;; inherently superior to using the arrow keys... but knowing how to
;; do this can't hurt). The mode causes a pre-specified help file to
;; be shown whenever an arrow key is pressed, creating a convenient
;; method for reminding users how to use Emacs core set of
;; commands. The help window appears above, below, left, or right of
;; the current window depending on which corresponding arrow key was
;; pressed. The help file can be user-edited to allow for change as
;; new skills are learned.

;; This global variable determines which help file is visited by these
;; functions. This should be set in one of your init files:
;; (setq noob-arrows-help-file
;;       "~/.emacs.d/plugins/noob-arrows-mode.el/noob-arrows-help-file.txt")

(defun noob-arrows-help-content ()
  (if (and 
       (boundp 'noob-arrows-help-file)
       (file-exists-p noob-arrows-help-file))
      ;; If the user has supplied a valid help file, insert the
      ;; contents of it into the help buffer
      (insert-file-contents noob-arrows-help-file nil nil nil t)
    ;; Else, display a custom message
    (progn
      (erase-buffer)
      (insert 
"Point (cursor) Commands
=======================
C-f	: Forward character
C-b	: Backward character
M-f	: Forward word
M-b	: Backward word
C-a	: Move cursor to beginning of current line
C-e	: Move cursor to end of current line
M-a	: Backward sentence
M-e	: Forward sentence

Buffer & File Commands
======================
C-x C-f : Open file
C-x C-s : Save current buffer
C-x C-w : Save buffer as
C-x b	: Switch buffers (default is last buffer visited)
C-x C-b : List buffers
C-x k	: Kill buffer (default is current buffer)

Window Commands
===============
C-x 1	: Close all windows except the current window
C-x 2	: Split current window horizontally
C-x 3	: Split current window vertically
C-x 0   : Close the current window
C-x o   : Switch to next window"))))

(defun noob-arrows-toggle-help (window-location)
  "Opens an Emacs keyboard shortcut guide in `window-location', which
can be `above', `below', `left', or `right' the current
  window. Calling the function again hides the help window"
    (let ((calling-buffer (current-buffer))
	  (help-window (get-buffer-window "emacs-help")))
      (save-excursion
	(if (equal help-window nil)
	  (progn
	    (if (equal (get-buffer "emacs-help") nil)
		(generate-new-buffer "emacs-help"))
	    (switch-to-buffer "emacs-help")
	    (cond 
	     ((string= "left" window-location)
	      (split-window (selected-window) nil t)
	      (noob-arrows-help-content)
	      (other-window 1))
	     ((string= "right" window-location)
	      (split-window (selected-window) nil t)
	      (noob-arrows-help-content))
	     ((string= "below" window-location)
	      (split-window (selected-window) nil nil)
	      (noob-arrows-help-content))
	     ((string= "above" window-location)
	      (split-window (selected-window) nil nil)
	      (noob-arrows-help-content)
	      (other-window 1)))
	    (switch-to-buffer calling-buffer)
	    (message "Hide help with additional arrow press or C-x 1"))
	  ;; If the help buffer is already showing, hide it
	  (delete-window help-window)))))

(define-minor-mode noob-arrows-mode
  "Noob training minor mode that replaces arrow keys with help
functions" 
  nil " noob " 
  ;; Keymap for noob-arrows-mode
  '(([left] . 
     (lambda () 
       (interactive) 
       (noob-arrows-toggle-help "left")))
    ([right] . 
     (lambda () 
       (interactive) 
       (noob-arrows-toggle-help "right")))
    ([up] . 
     (lambda () 
       (interactive) 
       (noob-arrows-toggle-help "above")))
    ([down] . 
     (lambda () 
       (interactive) 
       (noob-arrows-toggle-help "below"))))
  :global t)

(define-globalized-minor-mode global-noob-arrows-mode
  noob-arrows-mode noob-arrows-mode)

(provide 'noob-arrows-mode)

;;; noob-arrows-mode.el ends here
