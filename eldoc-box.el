;;; eldoc-box.el --- Display documentation in childframe      -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 Sebastien Chapuis, 2018 Yuan Fu

;; Version: 1.8

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; Maintainer: Yuan Fu <casouri@gmail.com>
;; Contributors:
;;   João Távora <joaotavora@gmail.com>
;; URL: https://github.com/casouri/eldoc-box
;; Package-Requires: ((emacs "27.1"))

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
;; See documentation in README.org or visit homepage

;;; Code:
;;

(require 'cl-lib)
(require 'seq)

;;;; Userland
;;;;; Variable
(defgroup eldoc-box nil
  "Display Eldoc docs in a pretty child frame."
  :prefix "eldoc-box-"
  :group 'eldoc)

(defface eldoc-box-border '((((background dark)) . (:background "white"))
                            (((background light)) . (:background "black")))
  "The border color used in childframe.")

(defface eldoc-box-body '((t . (:background unspecified)))
  "Body face used in documentation childframe.")

(defcustom eldoc-box-only-multi-line nil
  "If non-nil, only use childframe when there are more than one line."
  :type 'boolean)

(defcustom eldoc-box-cleanup-interval 1
  "After this amount of seconds will eldoc-box attempt to cleanup the childframe.
E.g. if it is set to 1, the childframe is cleared 1 second after
you moved the point to somewhere else (that doesn't have a doc to show).
This doesn't apply to `eldoc-box-hover-at-point-mode',
in that mode the childframe is cleared as soon as point moves."
  :type 'number)

(defcustom eldoc-box-clear-with-C-g nil
  "If set to non-nil, eldoc-box clears childframe on \\[keyboard-quit]."
  :type 'boolean)

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
    (right-fringe . 3)
    (left-fringe . 3)
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
    (desktop-dont-save . t)
    (tab-bar-lines . 0))
  "Frame parameters used to create the frame.")

(defcustom eldoc-box-max-pixel-width 800
  "Maximum width of doc childframe in pixel.
Consider your machine's screen's resolution when setting this variable.
Set it to a function with no argument
if you want to dynamically change the maximum width."
  :type 'number)

(defcustom eldoc-box-max-pixel-height 700
  "Maximum height of doc childframe in pixel.
Consider your machine's screen's resolution when setting this variable.
Set it to a function with no argument
if you want to dynamically change the maximum height."
  :type 'number)

(defcustom eldoc-box-offset '(16 16 16)
  "Sets left, right & top offset of the doc childframe.
Its value should be a list: (left right top)"
  :type '(list
          (integer :tag "Left")
          (integer :tag "Right")
          (integer :tag "Top")))

(defvar eldoc-box-position-function #'eldoc-box--default-upper-corner-position-function
  "Eldoc-box uses this function to set childframe's position.
This should be a function that returns a (X . Y) cons cell.
It will be passes with two arguments: WIDTH and HEIGHT of the childframe.")

(defcustom eldoc-box-fringe-use-same-bg t
  "T means fringe's background color is set to as same as that of default."
  :type 'boolean)

(defvar eldoc-box-buffer-hook nil
  "Hook run after buffer for doc is setup.
Run inside the new buffer.")

(defvar eldoc-box-frame-hook nil
  "Hook run after doc frame is setup but just before it is made visible.
Each function runs inside the new frame and receives the main frame as argument.")

(defcustom eldoc-box-self-insert-command-list '(self-insert-command outshine-self-insert-command)
  "Commands in this list are considered `self-insert-command' by eldoc-box.
See `eldoc-box-inhibit-display-when-moving'."
  :type '(repeat symbol))

;;;;; Function
(defvar eldoc-box--inhibit-childframe nil
  "If non-nil, inhibit display of childframe.")

(defvar eldoc-box--frame nil ;; A backstage variable
  "The frame to display doc.")

(defun eldoc-box-quit-frame ()
  "Hide documentation childframe."
  (interactive)
  (when (and eldoc-box--frame (frame-live-p eldoc-box--frame))
    (make-frame-invisible eldoc-box--frame t)))

(defun eldoc-box--enable ()
  "Enable eldoc-box hover.
Intended for internal use."
  (add-function :before-while (local 'eldoc-message-function)
                #'eldoc-box--eldoc-message-function)
  (when eldoc-box-clear-with-C-g
    (advice-add #'keyboard-quit :before #'eldoc-box-quit-frame)))

(defun eldoc-box--disable ()
  "Disable eldoc-box hover.
Intended for internal use."
  (remove-function (local 'eldoc-message-function) #'eldoc-box--eldoc-message-function)
  (advice-remove #'keyboard-quit #'eldoc-box-quit-frame)
  ;; if minor mode is turned off when childframe is visible
  ;; hide it
  (when eldoc-box--frame
    (delete-frame eldoc-box--frame)
    (setq eldoc-box--frame nil)))

;;;;; Help at point

(defvar eldoc-box--help-at-point-last-point 0
  "This point cache is used by clean up function.
If (point) != last point, cleanup frame.")

(defun eldoc-box--help-at-point-cleanup ()
  "Try to clean up the childframe made by eldoc-box hack."
  (if (or (eq (point) eldoc-box--help-at-point-last-point)
          ;; don't clean up when the user clicks childframe
          (eq (selected-frame) eldoc-box--frame))
      (run-with-timer 0.1 nil #'eldoc-box--help-at-point-cleanup)
    (eldoc-box-quit-frame)
    (kill-local-variable 'eldoc-display-functions)))

(defun eldoc-box-help-at-point ()
  "Display documentation of the symbol at point."
  (interactive)
  (let ((eldoc-box-position-function
         #'eldoc-box--default-at-point-position-function))
    (eldoc-box--display
     (with-current-buffer eldoc--doc-buffer
       (buffer-string))))
  (setq eldoc-box--help-at-point-last-point (point))
  (run-with-timer 0.1 nil #'eldoc-box--help-at-point-cleanup))

;;;; Backstage
;;;;; Variable
(defvar eldoc-box--buffer " *eldoc-box*"
  "The buffer used to display documentation.")

;;;;; Function

;; Please compiler.
(defvar eldoc-box-hover-mode)
(defun eldoc-box--display (str)
  "Display STR in childframe.
STR has to be a proper documentation, not empty string, not nil, etc."
  (let ((doc-buffer (get-buffer-create eldoc-box--buffer)))
    (with-current-buffer doc-buffer
      (setq mode-line-format nil)
      (when (bound-and-true-p global-tab-line-mode)
        (setq tab-line-format nil))
      ;; without this, clicking childframe will make doc buffer the current buffer
      ;; and `eldoc-box--maybe-cleanup' in `eldoc-box--cleanup-timer' will clear the childframe
      (buffer-face-set 'eldoc-box-body)
      (setq eldoc-box-hover-mode t)
      (erase-buffer)
      (insert str)
      (goto-char (point-min))
      (visual-line-mode)
      (run-hook-with-args 'eldoc-box-buffer-hook))
    (eldoc-box--get-frame doc-buffer)))


(defun eldoc-box--window-side ()
  "Return the side of the selected window.
Symbol 'left if the selected window is on the left,'right if on the right.
Return 'left if there is only one window."
  (let ((left-window(window-at 0 0)))
    (if (eq left-window (selected-window))
        'left
      'right)))

(defun eldoc-box--default-upper-corner-position-function (width _)
  "The default function to set childframe position.
Used by `eldoc-box-position-function'.
Position is calculated base on WIDTH and HEIGHT of childframe text window"
  (pcase-let ((`(,offset-l ,offset-r ,offset-t) eldoc-box-offset))
    (cons (pcase (eldoc-box--window-side) ; x position + offset
            ;; display doc on right
            ('left (- (frame-outer-width (selected-frame)) width offset-r))
            ;; display doc on left
            ('right offset-l))
          ;; y position + v-offset
          offset-t)))

(defun eldoc-box--point-position-relative-to-native-frame (&optional point window)
  "Return (X . Y) as the coordinate of POINT in WINDOW.
The coordinate is relative to the native frame.

WINDOW nil means use selected window."
  (let* ((pos (pos-visible-in-window-p point window t))
         (x (car pos))
         (en (frame-char-width))
         (y (cadr pos))
         (edges (window-edges window nil nil t)))
    ;; HACK: for unknown reasons we need to add en to x position
    (cons (+ x (car edges) en)
          (+ y (cadr edges)))))

(defun eldoc-box--default-at-point-position-function-1 (width height)
  "See `eldoc-box--default-at-point-position-function' for WIDTH & HEIGHT docs."
  (let* ((point-pos (eldoc-box--point-position-relative-to-native-frame))
         ;; calculate point coordinate relative to native frame
         ;; because childframe coordinate is relative to native frame
         (x (car point-pos))
         (y (cdr point-pos))
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

(defun eldoc-box--default-at-point-position-function (width height)
  "Set `eldoc-box-position-function' to this function.
To have childframe appear under point.  Position is calculated
base on WIDTH and HEIGHT of childframe text window."
  (let* ((pos (eldoc-box--default-at-point-position-function-1 width height))
         (x (car pos))
         (y (cdr pos)))
    (cons (or (eldoc-box--at-point-x-by-company) x)
          y)))
(defun eldoc-box--update-childframe-geometry (frame window)
  "Update the size and the position of childframe.
FRAME is the childframe, WINDOW is the primary window."
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
    (set-frame-position frame (car pos) (cdr pos))))

(defun eldoc-box--inhibit-childframe-for (sec)
  "Inhibit display of childframe for SEC seconds after Emacs is idle again."
  (unless eldoc-box--inhibit-childframe
    (setq eldoc-box--inhibit-childframe t)
    (eldoc-box-quit-frame)
    (run-with-idle-timer sec nil
                    (lambda ()
                      (setq eldoc-box--inhibit-childframe nil)))))

(defun eldoc-box--follow-cursor ()
  "Make childframe follow cursor in at-point mode."
  (unless eldoc-box--inhibit-childframe
    (if (member this-command eldoc-box-self-insert-command-list)
        (progn (when (frame-live-p eldoc-box--frame)
                 (eldoc-box--update-childframe-geometry
                  eldoc-box--frame (frame-selected-window eldoc-box--frame))))
      ;; if not typing, inhibit display
      (eldoc-box--inhibit-childframe-for 0.5))))

(defun eldoc-box--get-frame (buffer)
  "Return a childframe displaying BUFFER.
Checkout `lsp-ui-doc--make-frame', `lsp-ui-doc--move-frame'."
  (if eldoc-box--inhibit-childframe
      ;; if inhibit display, do nothing
      eldoc-box--frame
    (let* ((after-make-frame-functions nil)
           (before-make-frame-hook nil)
           (parameter (append eldoc-box-frame-parameters
                              `((default-minibuffer-frame . ,(selected-frame))
                                (minibuffer . ,(minibuffer-window))
                                (left-fringe . ,(frame-char-width)))))
           window frame
           (main-frame (selected-frame)))
      (if (and eldoc-box--frame (frame-live-p eldoc-box--frame))
          (progn (setq frame eldoc-box--frame)
                 (setq window (frame-selected-window frame))
                 ;; in case the main frame changed
                 (set-frame-parameter frame 'parent-frame main-frame))
        (setq window (display-buffer-in-child-frame
                      buffer
                      `((child-frame-parameters . ,parameter))))
        (setq frame (window-frame window)))
      ;; workaround
      ;; (set-frame-parameter frame 'left-fringe (alist-get 'left-fringe eldoc-box-frame-parameters))
      ;; (set-frame-parameter frame 'right-fringe (alist-get 'right-fringe eldoc-box-frame-parameters))

      (set-face-attribute 'fringe frame :background nil :inherit 'eldoc-box-body)
      (set-window-dedicated-p window t)
      (redirect-frame-focus frame (frame-parent frame))
      (set-face-attribute 'internal-border frame :inherit 'eldoc-box-border)
      (when (facep 'child-frame-border)
        (set-face-background 'child-frame-border
                             (face-attribute 'eldoc-box-border :background)
                             frame))
      ;; set size
      (eldoc-box--update-childframe-geometry frame window)
      (setq eldoc-box--frame frame)
      (with-selected-frame frame
        (run-hook-with-args 'eldoc-box-frame-hook main-frame))
      (make-frame-visible frame))))


;;;;; ElDoc

(defvar eldoc-box--cleanup-timer nil
  "The timer used to cleanup childframe after ElDoc.")

(defvar eldoc-box--last-point 0
  ;; used in `eldoc-box--maybe-cleanup'
  "Last point when eldoc-box showed childframe.")

;; Please compiler.
(defvar eldoc-box-hover-at-point-mode)
(defun eldoc-box--maybe-cleanup ()
  "Clean up after ElDoc."
  ;; timer is global, so this function will be called outside
  ;; the buffer with `eldoc-box-hover-mode' enabled
  (if (and (frame-parameter eldoc-box--frame 'visibility)
           (or (and (not eldoc-last-message) ; 1
                    (not (eq (point) eldoc-box--last-point)) ; 2
                    (not (eq (current-buffer) (get-buffer eldoc-box--buffer)))) ; 3
               (not (or eldoc-box-hover-mode eldoc-box-hover-at-point-mode)))) ; 4
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

(defun eldoc-box--eldoc-message-function (str &rest args)
  "Front-end for eldoc.
Display STR in childframe and ARGS works like `message'."
  (when (and (stringp str) (not (equal str "")))
    (let* ((doc (string-trim-right (apply #'format str args)))
           (single-line-p (and eldoc-box-only-multi-line
                               (eq (cl-count ?\n doc) 0))))
      (unless single-line-p
        (eldoc-box--display doc)
        (setq eldoc-box--last-point (point))
        ;; Why a timer? ElDoc is mainly used in minibuffer,
        ;; where the text is constantly being flushed by other commands
        ;; so ElDoc doesn't try very hard to cleanup
        (when eldoc-box--cleanup-timer (cancel-timer eldoc-box--cleanup-timer))
        ;; this function is also called by `eldoc-pre-command-refresh-echo-area'
        ;; in `pre-command-hook', which means the timer is reset before every
        ;; command if `eldoc-box-hover-mode' is on and `eldoc-last-message' is not nil.
        (setq eldoc-box--cleanup-timer
              (run-with-timer eldoc-box-cleanup-interval nil #'eldoc-box--maybe-cleanup)))
      single-line-p)))

;;;###autoload
(define-minor-mode eldoc-box-hover-mode
  "Displays hover documentations in a childframe.
The default position of childframe is upper corner."
  :lighter " ELDOC-BOX"
  (if eldoc-box-hover-mode
      (progn (when eldoc-box-hover-at-point-mode
               (eldoc-box-hover-at-point-mode -1))
             (eldoc-box--enable))
    (eldoc-box--disable)))

;;;###autoload
(define-minor-mode eldoc-box-hover-at-point-mode
  "A convenient minor mode to display doc at point.
You can use \[keyboard-quit] to hide the doc."
  :lighter " ELDOC-BOX"
  (if eldoc-box-hover-at-point-mode
      (progn (when eldoc-box-hover-mode
               (eldoc-box-hover-mode -1))
             (setq-local eldoc-box-position-function
                         #'eldoc-box--default-at-point-position-function)
             (setq-local  eldoc-box-clear-with-C-g t)
             (remove-hook 'pre-command-hook #'eldoc-pre-command-refresh-echo-area t)
             (add-hook 'post-command-hook #'eldoc-box--follow-cursor t t)
             (eldoc-box--enable))
    (eldoc-box--disable)
    (add-hook 'pre-command-hook #'eldoc-pre-command-refresh-echo-area t)
    (remove-hook 'post-command-hook #'eldoc-box--follow-cursor t)
    (kill-local-variable 'eldoc-box-position-function)
    (kill-local-variable 'eldoc-box-clear-with-C-g)))

;;;; Eglot helper

(eval-and-compile
  (require 'jsonrpc)
  (when (require 'eglot nil t)

    (defvar eglot--managed-mode)
    (declare-function eglot--dbind "eglot.el")
    (declare-function eglot--hover-info "eglot.el")
    (declare-function eglot--current-server-or-lose "eglot.el")
    (declare-function eglot--TextDocumentPositionParams "eglot.el")
    (declare-function eglot--error "eglot.el")
    (declare-function jsonrpc-request "jsonrpc")


    (defun eldoc-box-eglot-help-at-point ()
      "Display documentation of the symbol at point."
      (interactive)
      (when eglot--managed-mode
        (let ((eldoc-box-position-function #'eldoc-box--default-at-point-position-function))
          (let ((hover-info
                 (eglot--dbind ((Hover) contents range)
                     (jsonrpc-request (eglot--current-server-or-lose) :textDocument/hover
                                      (eglot--TextDocumentPositionParams))
                   (when (seq-empty-p contents) (eglot--error "No hover info here"))
                   (eglot--hover-info contents range))))
            (if hover-info
                (eldoc-box--display hover-info)
              (eglot--error "No hover info here"))))
        (setq eldoc-box--help-at-point-last-point (point))
        (run-with-timer 0.1 nil #'eldoc-box--help-at-point-cleanup)))))

;;;; Comany compatibility
;;

;; see also `eldoc-box--default-at-point-position-function'

;; please compiler
(defvar company-pseudo-tooltip-overlay)

(defun eldoc-box--at-point-x-by-company ()
  "Return the x position that accommodates company's popup."
  (if (and (featurep 'company) company-pseudo-tooltip-overlay)
      (+ (* (frame-char-width)
            (+ (overlay-get company-pseudo-tooltip-overlay 'company-width)
               (overlay-get company-pseudo-tooltip-overlay 'company-column)))
         (or (line-number-display-width t) 0))
    nil))

(provide 'eldoc-box)

;;; eldoc-box.el ends here
