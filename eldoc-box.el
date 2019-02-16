;;; eldoc-box.el --- Display documentation in childframe      -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 Sebastien Chapuis, 2018 Yuan Fu

;; Version: 1.4

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; Maintainer: Yuan Fu <casouri@gmail.com>
;; Contributors:
;;   João Távora <joaotavora@gmail.com>
;; URL: https://github.com/casouri/eldoc-box
;; Package-Requires: ((emacs "26.1"))

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;;  Made a lot of change to use it for ElDoc
;;
;;
;; 1.How does eldoc work:
;;
;; Entry function: ‘eldoc-print-current-symbol-info’
;; (there is a timer that set it up)
;;
;; 1.1 What it does:
;;
;; if we should display a message now [1]:
;;    get doc from ‘eldoc-documentation-function’
;;
;;    if it is non-nil:
;;       display it (with ‘eldoc-message-function’)
;;
;;    else if ‘eldoc-last-message’ is non-nil:
;;       pass nil to message function
;;
;;    else (both are nil): do nothing
;;
;;    finally set ‘eldoc-last-message’ to the doc we get,
;;    whether it is non-nil or nil
;;
;; [1]: 1) we are not in the middle of some command, and 2) last command
;; is in ‘eldoc-message-commands’, i.e., motion command, self-insert, etc
;;
;;
;; In conclusion:
;;
;;         last-time  this-time  do
;; case1:  nil        nil        nothing
;; case2:  t          nil        pass nil to message function
;; case3:  nil/t      t          display it
;;
;; if you got time to kill, look at ‘eldoc-message’ for a nice little puzzle
;;
;;
;; 1.2 When does it run:
;;
;; Nothing fancy, run in an idle timer
;; (a timer scheduler in ‘post-command-hook’? maybe a bit overkill)
;;
;; 2. What we do:
;;
;; 2.1 What we want:
;;
;; 1) display like normal minibuffer doc
;; 2) be able to disappear AS SOON AS cursor moves to undoc-able place
;; 3) doesn’t disappear and reappear when moving inside the same symbol
;;    (displaying the same doc)
;; 4) (for displaying around cursor) doesn’t move position when displaying
;;    the same doc
;;
;; 2.2 What information do we need to know:
;;
;; 1) doc of this time
;; 2) doc of last time
;;
;; 2.3 When do we need to run
;;
;; after each command
;;
;; 2.4 How do we display
;;
;;         last-time  this-time  do
;; case1:  nil        nil        nothing
;; case2:  t          nil        remove display
;; case3:  nil        t          display it
;; case4:  t     !=   t          display it
;; case5:  t     ==   t          nothing (actually, display the new one
;;                                        but don’t change position)[2]
;;
;; [2]: for bold face for current component, etc
;;
;;
;; 3. Problem with eglot
;;
;; Eglot is async, so it doesn’t return doc immediately
;;
;; 3.1 How does it work
;;
;; 1) eldoc calls it for doc, it returns nil immediately
;; 2) what ‘eldoc-message’ does:
;;         last-time  this-time  do
;; case1:  nil        nil        nothing
;; case2:  t          nil        remove display
;; 3) After a very short time, it calls ‘eldoc-message’ with doc:
;; what ‘eldoc-message’ does:
;; case2:  t          nil        pass nil to message function
;; case3:  nil/t      t          display it
;;
;; Conclusion:
;;
;; 1) I don’t think “disappear immediately” and no blink (disappear & reappear)
;;    can be achieved in the same time for eglot, I go for no blink, so:
;; 2) Use timer to clean up, display function (us) don’t hide doc in 2)
;;    even if receives nil


;;; Code:
;;

(require 'cl-lib)

;;;; Userland
;;;;; Variable
(defgroup eldoc-box nil
  "Display Eldoc docs in a pretty child frame."
  :prefix "eldoc-box-"
  :group 'eldoc)

(defface eldoc-box-border '((((background dark)) . (:background "white"))
                            (((background light)) . (:background "black")))
  "The border color used in childframe.")

(defface eldoc-box-body '((t . (:background nil)))
  "Body face used in eglot doc childframe. Only :background and :font are used.")

(defvar eldoc-box-cleanup-interval 1
  "After this amount of seconds will eldoc-box attempt to cleanup the childframe.
E.g. if it is set to 1, the childframe is cleared 1 second after
you moved the point to somewhere else (that doesn't have a doc to show).
This doesn't apply to `eldoc-box-hover-at-point-mode',
in that mode the childframe is cleared as soon as point moves.")

(defvar eldoc-box-clear-with-C-g nil
  "If set to non-nil, eldoc-box clears childframe when you hit \C-g.")

(defvar eldoc-box-frame-parameters
  '(;; make the childframe unseen when first created
    (left . -1)
    (top . -1)
    (width  . 0)
    (height  . 0)

    (no-accept-focus . t)
    (no-focus-on-map . t)
    (min-width  . 0)
    (min-height  . 0)
    (internal-border-width . 1)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (right-fringe . 0)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (line-spacing . 0)
    (unsplittable . t)
    (undecorated . t)
    (visibility . nil)
    (mouse-wheel-frame . nil)
    (no-other-frame . t)
    (cursor-type . nil)
    (inhibit-double-buffering . t)
    (drag-internal-border . t)
    (no-special-glyphs . t)
    (desktop-dont-save . t))
  "Frame parameters used to create the frame.")

(defvar eldoc-box-max-pixel-width 800
  "Maximum width of doc childframe in pixel.
Consider your machine's screen's resolution when setting this variable.
Set it to a function with no argument
if you want to dynamically change the maximum width.")

(defvar eldoc-box-max-pixel-height 700
  "Maximum height of doc childframe in pixel.
Consider your machine's screen's resolution when setting this variable.
Set it to a function with no argument
if you want to dynamically change the maximum height.")

(defvar eldoc-box-position-function #'eldoc-box--default-upper-corner-position-function
  "Eldoc-box uses this function to set childframe's position.
This should be a function that returns a (X . Y) cons cell.
It will be passes with two arguments: WIDTH and HEIGHT of the childframe.")

;;;;; Function

(defmacro eldoc-box--if-eglot (then &rest else)
  "Do THEN if eglot is on, do ELSE if not."
  `(if (bound-and-true-p eglot--managed-mode)
       ,then
     ,@else))

(defvar eldoc-box--frame nil ;; A backstage variable
  "The frame to display doc.")

(defun eldoc-box-quit-frame ()
  "Hide childframe used by eglot doc."
  (interactive)
  (when eldoc-box--frame
    (make-frame-invisible eldoc-box--frame t)))

(defun eldoc-box--bind-c-g ()
  "Advice C-g to hide frame."
  (add-function :before (local 'keyboard-quit) #'eldoc-box-quit-frame))

(defun eldoc-box--unbind-c-g ()
  "Remove advice on C-g that hides frame."
  (remove-function (local 'keyboard-quit) #'eldoc-box-quit-frame))

;;;###autoload
(define-minor-mode eldoc-box-hover-mode
  "Displays hover documentations in a childframe. This mode is buffer local."
  :lighter " ELDOC-BOX"
  (if eldoc-box-hover-mode
      (progn (add-function :before-until (local 'eldoc-message-function)
                           #'eldoc-box--eldoc-message-function)
             (when eldoc-box-clear-with-C-g
               (eldoc-box--bind-c-g))
             (eldoc-box--disable-eldoc-pre-command-hook)
             (eldoc-box--add-spy))

    (eldoc-box--remove-spy)
    (eldoc-box--enable-eldoc-pre-command-hook)
    (eldoc-box--unbind-c-g)
    (remove-function (local 'eldoc-message-function) #'eldoc-box--eldoc-message-function)
    (advice-remove #'keyboard-quit #'eldoc-box-quit-frame)
    ;; if minor mode is turned off when childframe is visible
    ;; hide it
    (when eldoc-box--frame
      (delete-frame eldoc-box--frame)
      (setq eldoc-box--frame nil))))

;;;###autoload
(define-minor-mode eldoc-box-hover-at-point-mode
  "A convenient minor mode to display doc at point.
You can use C-g to hide the doc."
  :lighter ""
  (if eldoc-box-hover-mode
      (if eldoc-box-hover-at-point-mode
          (progn (setq-local
                  eldoc-box-position-function
                  #'eldoc-box--default-at-point-position-function))

        (kill-local-variable 'eldoc-box-position-function))
    (message "Enable eldoc-box-hover-mode first")))

;;;; Backstage
;;;;; Variable
(defvar eldoc-box--buffer " *eldoc-box*"
  "The buffer used to display eglot doc.")

;;;;; Function

(defun eldoc-box--display (str &optional dont-move)
  "Display STR in childframe.
IF DONT-MOVE is non-nil, doesn’t move position of frame."
  (unless (equal str "") ; WORKAROUND lsp returns empty string from time to time
    (let ((doc-buffer (get-buffer-create eldoc-box--buffer)))
      (with-current-buffer doc-buffer
        (setq mode-line-format nil)
        ;; without this, clicking childframe will make doc buffer the current buffer
        ;; and `eldoc-box--maybe-cleanup' in `eldoc-box--cleanup-timer' will clear the childframe
        (setq eldoc-box-hover-mode t)
        (erase-buffer)
        (insert str)
        (goto-char (point-min)))
      (eldoc-box--get-frame doc-buffer dont-move))))


(defun eldoc-box--window-side ()
  "Return 'left if the selected window is on the left,
'right if on the right. Return 'left if there is only one window."
  (let ((left-window(window-at 0 0)))
    (if (eq left-window (selected-window))
        'left
      'right)))

(defun eldoc-box--default-upper-corner-position-function (width _)
  "The default function to set childframe position.
Used by `eldoc-box-position-function'.
Position is calculated base on WIDTH and HEIGHT of childframe text window"
  (cons (pcase (eldoc-box--window-side) ; x position + a little padding (16)
          ;; display doc on right
          ('left (- (frame-outer-width (selected-frame)) width 16))
          ;; display doc on left
          ('right 16))
        ;; y position + a little padding (16)
        16))

(defun eldoc-box--point-position-relative-to-native-frame (&optional position window)
  "Return (X . Y) as the coordinate of POSITION in WINDOW.
The coordinate is relative to the native frame.

WINDOW nil means use selected window."
  (let* ((window (window-normalize-window window t))
	 (pos-in-window
	  (pos-visible-in-window-p
	   (or position (window-point window)) window t)))
    (when pos-in-window
      ;; change absolute to relative to native frame
      (let ((edges (window-edges window t nil t)))
	(cons (+ (nth 0 edges) (nth 0 pos-in-window))
	      (+ (nth 1 edges) (nth 1 pos-in-window)))))))

(defun eldoc-box--default-at-point-position-function (width height)
  "Set `eldoc-box-position-function' to this function to have childframe appear under point.
Position is calculated base on WIDTH and HEIGHT of childframe text window"
  (let* ((point-pos (eldoc-box--point-position-relative-to-native-frame))
         ;; calculate point coordinate relative to native frame
         ;; because childframe coordinate is relative to native frame
         (x (car point-pos))
         (y (cdr point-pos))
         ;; (en (frame-char-width))
         (em (frame-char-height)))
    (cons (if (< (- (frame-inner-width) width) x)
              ;; space on the right of the pos is not enough
              ;; put to left
              (max 0 (- x width))
            ;; normal, just return x
            x)
          (if (< (- (frame-inner-height) height) y)
              ;; space under the pos is not enough
              ;; put above
              (max 0 (- y height))
            ;; normal, just return y + em
            (+ y em)))))

(defun eldoc-box--get-frame (buffer &optional dont-move)
  "Return a childframe displaying BUFFER.
Checkout `lsp-ui-doc--make-frame', `lsp-ui-doc--move-frame'.
If DONT-MOVE non-nil, dont’ move frame position."
  (let* ((after-make-frame-functions nil)
         (before-make-frame-hook nil)
         (parameter (append eldoc-box-frame-parameters
                            `((default-minibuffer-frame . ,(selected-frame))
                              (minibuffer . ,(minibuffer-window))
                              (left-fringe . ,(frame-char-width)))))
         window frame
         (main-frame (selected-frame)))
    (if eldoc-box--frame
        (progn (setq frame eldoc-box--frame)
               (setq window (frame-selected-window frame)))
      (setq window (display-buffer-in-child-frame
                    buffer
                    `((child-frame-parameters . ,parameter))))
      (setq frame (window-frame window)))
    (set-window-dedicated-p window t)
    (redirect-frame-focus frame (frame-parent frame))
    (set-face-attribute 'internal-border frame :inherit 'eldoc-box-border)
    (set-face-attribute 'default frame
                        :background (face-attribute 'eldoc-box-body :background main-frame)
                        :font (face-attribute 'eldoc-box-body :font main-frame))
    ;; set size
    (let* ((size
            (window-text-pixel-size
             window nil nil
             (if (functionp eldoc-box-max-pixel-width) (funcall eldoc-box-max-pixel-width) eldoc-box-max-pixel-width)
             (if (functionp eldoc-box-max-pixel-height) (funcall eldoc-box-max-pixel-height) eldoc-box-max-pixel-height)
             t))
           (width (car size))
           (height (cdr size))
           (width (+ width (frame-char-width frame))) ; add margin
           (frame-resize-pixelwise t)
           (pos (funcall eldoc-box-position-function width height)))
      (set-frame-size frame width height t)
      ;; move position
      (unless dont-move
        (set-frame-position frame (car pos) (cdr pos))))
    (setq eldoc-box--frame frame)
    (make-frame-visible frame)))


;;;;; ElDoc

(defvar eldoc-box--cleanup-timer nil
  "The timer used to cleanup childframe after ElDoc.")

(defvar eldoc-box--last-point 0
  ;; used in `eldoc-box--maybe-cleanup'
  "Last point when eldoc-box showed childframe.")

(defun eldoc-box--maybe-cleanup ()
  "Clean up after ElDoc."
  ;; timer is global, so this function will be called outside
  ;; the buffer with `eldoc-box-hover-mode' enabled
  (if (and (frame-parameter eldoc-box--frame 'visibility)
           (or (and (not eldoc-last-message) ; 1
                    (not (eq (point) eldoc-box--last-point)) ; 2
                    (not (eq (current-buffer) (get-buffer eldoc-box--buffer)))) ; 3
               (not eldoc-box-hover-mode))) ; 4
      ;; 1. Obviously, last-message nil means we are not on a valid symbol anymore.
      ;; 2. Or are we? If you scroll the childframe with mouse wheel
      ;; `eldoc-pre-command-refresh-echo-area' will set `eldoc-last-message' to nil.
      ;; Without the point test, this function, called by `eldoc-box--cleanup-timer'
      ;; will clear the doc frame, not good
      ;; 3. If scrolling can't satisfy you and you clicked the childframe
      ;; both 1. and 2. are satisfied. 3. is the last hope to prevent this function
      ;; from clearing your precious childframe. There is another safety pin in
      ;; `eldoc-box--display' that works with 3.
      ;; 4. Sometimes you switched buffer when childframe is on.
      ;; it wouldn't go away unless you goes back and let eldoc shut it off.
      ;; So if we are not in `eldoc-box-hover-mode', clear childframe
      (eldoc-box-quit-frame)
    ;; so you didn't clear the doc frame this time, and the last timer has ran out
    ;; setup another one to make sure the doc frame is cleared
    ;; once the condition above it met
    (setq eldoc-box--cleanup-timer
          (run-with-timer eldoc-box-cleanup-interval nil #'eldoc-box--maybe-cleanup))))

(defun eldoc-box--disable-eldoc-pre-command-hook ()
  "Disable eldoc’s pre-command-hook.
It ensures docs in minibuffer doesn’t goes away because of motion
commands. We don’t need that."
  (add-function :override (local 'eldoc-pre-command-refresh-echo-area) #'ignore))

(defun eldoc-box--enable-eldoc-pre-command-hook ()
  "See ‘eldoc-box--disable-eldoc-pre-command-hook’."
  (remove-function (local 'eldoc-pre-command-refresh-echo-area) #'ignore))

(defvar eldoc-box--real-last-message nil
  "More in ‘eldoc-box--eldoc-spy’.")

(defun eldoc-box--eldoc-spy (&rest _)
  "Advice this function before ‘eldoc-message’ and we know the real ‘last-message’."
  ;; because, you know, eldoc-message sets doc it recieved to ‘eldoc-last-message’.
  ;; so the one we can see in display function is actually the new one,
  ;; the old one is gone.
  (setq eldoc-box--real-last-message eldoc-last-message)
  (print "woome"))

(defun eldoc-box--add-spy ()
  "Advice ‘eldoc-message’, more in ‘eldoc-box--eldoc-spy’."
  (add-function :before (local 'eldoc-message) #'eldoc-box--eldoc-spy))

(defun eldoc-box--remove-spy ()
  "Remove advice on ‘eldoc-message’, more in ‘eldoc-box--eldoc-spy’."
  (remove-function (local 'eldoc-message) #'eldoc-box--eldoc-spy))

(defun eldoc-box--display-message-do-the-right-thing (doc-last-time doc-this-time)
  "Display/remove doc depending on DOC-LAST-TIME and DOC-THIS-TIME."
  (eldoc-box--if-eglot
   ;; if eglot
   (if doc-this-time
       (eldoc-box--display doc-this-time))
   ;; if not
   ;;         last-time  this-time  do
   ;; case1:  nil        nil        nothing
   ;; case2:  t          nil        remove display
   ;; case3:  nil        t          display it
   ;; case4:  t     !=   t          display it
   ;; case5:  t     ==   t          actually, display new message
   ;;                               don’t move the position, for the bold face
   ;;                               change, etc
   ;;      case 2
   (cond ((and doc-last-time (not doc-this-time))
          (eldoc-box-quit-frame))
         ;;    case 3
         ((or (and (not doc-last-time) doc-this-time)
              ;; case 4
              (and (and doc-last-time doc-this-time)
                   (not (equal doc-last-time doc-this-time))))
          (eldoc-box--display doc-this-time))
         ((and doc-last-time doc-this-time
               (equal doc-last-time doc-this-time))
          (eldoc-box--display doc-this-time 'dont-move)))))

(defun eldoc-box--eldoc-message-function (str &rest args)
  "Front-end for eldoc. Display (or not) STR in childframe and ARGS works like `message'."
  (let ((doc-last-time eldoc-box--real-last-message)
        (doc-this-time (if str (apply #'format str args) nil)))
    (eldoc-box--display-message-do-the-right-thing doc-last-time doc-this-time))
  ;; this info is used by ‘eldoc-box--maybe-cleanup’
  (setq eldoc-box--last-point (point))
  ;; always try to clean up clean up timer, it doesn’t harm anyway
  (when eldoc-box--cleanup-timer
    (cancel-timer eldoc-box--cleanup-timer)
    (setq eldoc-box--cleanup-timer nil))
  (eldoc-box--if-eglot
   (setq eldoc-box--cleanup-timer
         (run-with-timer eldoc-box-cleanup-interval nil #'eldoc-box--maybe-cleanup)))
  t)

;;;; Eglot helper

(defvar eldoc-box-eglot-help-at-point-last-point 0
  "This point cache is used by clean up function.
If (point) != last point, cleanup frame.")

(defun eldoc-box--eglot-help-at-point-cleanup ()
  "Try to clean up the childframe made by eldoc-box hack."
  (if (or (eq (point) eldoc-box-eglot-help-at-point-last-point)
          ;; don't clean up when the user clicks childframe
          (eq (selected-frame) eldoc-box--frame))
      (run-with-timer 0.1 nil #'eldoc-box--eglot-help-at-point-cleanup)
    (eldoc-box-quit-frame)))

(defvar eglot--managed-mode)
(declare-function eglot--dbind "eglot.el")


(defun eldoc-box-eglot-help-at-point ()
  "Display documentation of the symbol at point."
  (interactive)
  (when eglot--managed-mode
    (let ((eldoc-box-position-function #'eldoc-box--default-at-point-position-function))
      (eldoc-box--display
       (eglot--dbind ((Hover) contents range)
                     (jsonrpc-request (eglot--current-server-or-lose) :textDocument/hover
                                      (eglot--TextDocumentPositionParams))
                     (when (seq-empty-p contents) (eglot--error "No hover info here"))
                     (eglot--hover-info contents range))))
    (setq eldoc-box-eglot-help-at-point-last-point (point))
    (run-with-timer 0.1 nil #'eldoc-box--eglot-help-at-point-cleanup)))

;;;; Company

(defun eldoc-box--move-up-for-company (&rest _)
  "Move (at point) doc up above current line to avoid clash with company pop up."
  (when (and eldoc-box-hover-at-point-mode eldoc-box--frame)
    (let ((current-x (frame-parameter eldoc-box--frame 'left))
          (current-y (frame-parameter eldoc-box--frame 'top)))
      (set-frame-position eldoc-box--frame current-x (- current-y (frame-char-height))))))

(defun eldoc-box--move-down-for-company (&rest _)
  "Move (at point) doc back down when company’s popup closes."
  (when (and eldoc-box-hover-at-point-mode eldoc-box--frame)
    (let ((current-x (frame-parameter eldoc-box--frame 'left))
          (current-y (frame-parameter eldoc-box--frame 'top)))
      (set-frame-position eldoc-box--frame current-x (+ current-y (frame-char-height))))))

(defun eldoc-box-setup-for-company ()
  "Set up integration with company."
  (interactive)
  (add-hook 'company-completion-started-hook #'eldoc-box--move-up-for-company)
  (add-hook 'company-completion-cancelled-hook #'eldoc-box--move-down-for-company)
  (add-hook 'company-completion-finished-hook #'eldoc-box--move-down-for-company))

;;;; Debug

(defun eldoc-box--print-last-message ()
  "Print ‘eldoc-last-message’."
  (message "last: %s this: %s" eldoc-box--real-last-message eldoc-last-message))

(defun eldoc-box--enable-print-last-message ()
  "Start printing ‘eldoc-last-message’ on every command."
  (interactive)
  (add-hook 'post-command-hook #'eldoc-box--print-last-message t t))

(defun eldoc-box--disable-print-last-message ()
  "Stop printing ‘eldoc-last-message’ on every command."
  (interactive)
  (remove-hook 'post-command-hook #'eldoc-box--print-last-message t))

(provide 'eldoc-box)

;;; eldoc-box.el ends here
