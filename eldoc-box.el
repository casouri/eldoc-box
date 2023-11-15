;;; eldoc-box.el --- Display documentation in childframe      -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Yuan Fu

;; Version: 1.11.1

;; Author: Yuan Fu <casouri@gmail.com>
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
;; Usage:
;;
;; There are three ways to use this package:
;;
;; 1. Enable ‘eldoc-box-hover-mode’. Emacs will show the documentation
;; of symbol at point in a children on the upper left or right corner.
;;
;; 2. Enable ‘eldoc-box-hover-at-point-mode’. Similar to
;; ‘eldoc-box-hover-mode’, but displays the childframe at point. (This
;; mode feels slower comparing to ‘eldoc-box-hover-mode’.)
;;
;; 3. Bind ‘eldoc-box-help-at-point’ to a key and bring up the
;; documentation childframe on-demand. This command requires Emacs 28
;; to work.
;;
;; Customization faces:
;;
;; - ‘eldoc-box-border’
;; - ‘eldoc-box-body’
;;
;; Hooks:
;;
;; - ‘eldoc-box-buffer-hook’
;; - ‘eldoc-box-frame-hook’
;;
;; Customize options:
;;
;; - ‘eldoc-box-max-pixel-width’
;; - ‘eldoc-box-max-pixel-height’
;; - ‘eldoc-box-only-multi-line’
;; - ‘eldoc-box-cleanup-interval’
;; - ‘eldoc-box-fringe-use-same-bg’
;; - ‘eldoc-box-self-insert-command-list’

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'pcase)
  (require 'seq))

;;;; Userland
;;;;; Variable
(defgroup eldoc-box nil
  "Display Eldoc docs in a pretty child frame."
  :prefix "eldoc-box-"
  :group 'eldoc)

(defface eldoc-box-border '((((background dark)) . (:background "white"))
                            (((background light)) . (:background "black")))
  "The border color used in childframe.")

(defface eldoc-box-body '((t . nil))
  "Body face used in documentation childframe.")

(defface eldoc-box-markdown-separator '((t . ( :strike-through t
                                               :height 0.4)))
  "Face for the separator line in Markdown.")

(defcustom eldoc-box-lighter " ELDOC-BOX"
  "Mode line lighter for all eldoc-box modes.
If the value is nil, no lighter is displayed."
  :type '(choice string
                 (const :tag "None" nil)))

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

(defcustom eldoc-box-doc-separator "\n\n"
  "The separator between documentation from different sources.

Since Emacs 28, Eldoc can combine documentation from different
sources, this separator is used to separate documentation from
different sources.

This separator is used for the documentation shown in
‘eldoc-box-bover-mode’ but not ‘eldoc-box-help-at-point’."
  :type 'string)

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
    (tab-bar-lines . 0)
    (tab-bar-lines-keep-state . 1))
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

The function is passed two arguments, WIDTH and HEIGHT of the
childframe, and should return a (X . Y) cons cell.")

(defvar eldoc-box-at-point-position-function #'eldoc-box--default-at-point-position-function
  "Eldoc-box uses this function to set childframe's position.
This function is used in ‘eldoc-box-help-at-point’ and in
‘eldoc-box-hover-at-point-mode’.

The function is passed two arguments, WIDTH and HEIGHT of the
childframe, and should return a (X . Y) cons cell.")

(defcustom eldoc-box-fringe-use-same-bg t
  "T means fringe's background color is set to as same as that of default."
  :type 'boolean)

(defvar eldoc-box-buffer-hook '(eldoc-box--prettify-markdown-separator
                                eldoc-box--replace-en-space
                                eldoc-box--remove-linked-images
                                eldoc-box--remove-noise-chars
                                eldoc-box--fontify-html
                                eldoc-box--condense-large-newline-gaps)
  "Hook run after buffer for doc is setup.
Run inside the new buffer. By default, it contains some Markdown
prettifiers, which see.")

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

(defvar-local eldoc-box--old-eldoc-functions nil
  "The original value of ‘eldoc-display-functions’.
The original value before enabling eldoc-box.")

(defun eldoc-box--enable ()
  "Enable eldoc-box hover.
Intended for internal use."
  (if (not (boundp 'eldoc-display-functions))
      (add-function :before-while (local 'eldoc-message-function)
                    #'eldoc-box--eldoc-message-function)

    (setq-local eldoc-box--old-eldoc-functions
                eldoc-display-functions)
    (setq-local eldoc-display-functions
                (cons 'eldoc-box--eldoc-display-function
                      (remq 'eldoc-display-in-echo-area
                            eldoc-display-functions))))

  (when eldoc-box-clear-with-C-g
    (advice-add #'keyboard-quit :before #'eldoc-box-quit-frame)))

(defun eldoc-box--disable ()
  "Disable eldoc-box hover.
Intended for internal use."
  (if (not (boundp 'eldoc-display-functions))
      (remove-function (local 'eldoc-message-function) #'eldoc-box--eldoc-message-function)

    (setq-local eldoc-display-functions
                (remq 'eldoc-box--eldoc-display-function
                      eldoc-display-functions))
    ;; If we removed eldoc-display-in-echo-area when enabling
    ;; eldoc-box, add it back.
    (when (memq 'eldoc-display-in-echo-area
                eldoc-box--old-eldoc-functions)
      (setq-local eldoc-display-functions
                  (cons 'eldoc-display-in-echo-area
                        eldoc-display-functions))))

  (advice-remove #'keyboard-quit #'eldoc-box-quit-frame)
  ;; If minor mode is turned off when the childframe is visible, hide it.
  (when eldoc-box--frame
    (delete-frame eldoc-box--frame)
    (setq eldoc-box--frame nil)))

;;;;; Help at point

(defvar eldoc-box--help-at-point-last-point 0
  "This point cache is used by the clean up function.
If point != last point, hide the childframe.")

(defun eldoc-box--help-at-point-cleanup ()
  "Try to clean up the childframe."
  (if (or (eq (point) eldoc-box--help-at-point-last-point)
          ;; Don't clean up when the user clicks into the childframe.
          (eq (selected-frame) eldoc-box--frame))
      (run-with-timer 0.1 nil #'eldoc-box--help-at-point-cleanup)
    (eldoc-box-quit-frame)))

;;;###autoload
(defun eldoc-box-help-at-point ()
  "Display documentation of the symbol at point."
  (interactive)
  (when (boundp 'eldoc--doc-buffer)
    (let ((eldoc-box-position-function
           eldoc-box-at-point-position-function))
      (eldoc-box--display
       (with-current-buffer eldoc--doc-buffer
         (buffer-string))))
    (setq eldoc-box--help-at-point-last-point (point))
    (run-with-timer 0.1 nil #'eldoc-box--help-at-point-cleanup)
    (when eldoc-box-clear-with-C-g
      (advice-add #'keyboard-quit :before #'eldoc-box-quit-frame))))

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
      (setq header-line-format nil)
      ;; WORKAROUND: (issue#66) If cursor-type is ‘box’, sometimes the
      ;; cursor is still shown for some reason.
      (setq-local cursor-type t)
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
Symbol ‘left’ if the selected window is on the left, ‘right’ if
on the right. Return ‘left’ if there is only one window."
  ;; Calculate the left and right distances to the frame edge of the
  ;; active window.  If the left distance is less than or equal to the
  ;; right distance, it indicates that the active window is on the left.
  ;; Otherwise, it is on the right.
  (let* ((window-left (nth 0 (window-absolute-pixel-edges)))
         (window-right (nth 2 (window-absolute-pixel-edges)))
         (frame-left (nth 0 (frame-edges)))
         (frame-right (nth 2 (frame-edges)))
         (distance-left (- window-left frame-left))
         (distance-right (- frame-right window-right)))
    ;; When `distance-left' equals `distance-right', it means there is
    ;; only one window in current frame, or the current active window
    ;; occupies the entire frame horizontally, return left.
    (if (<= distance-left distance-right)
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

(defvar eldoc-box--markdown-separator-display-props)

(defun eldoc-box--update-childframe-geometry (frame window)
  "Update the size and the position of childframe.
FRAME is the childframe, WINDOW is the primary window."
  ;; WORKAROUND: See issue#68. If there’s some text with a display
  ;; property of (space :width text) -- which is what we apply onto
  ;; markdown separators -- ‘window-text-pixel-size’ wouldn’t return
  ;; the correct value. Instead, it returns the current window width.
  ;; So now the childram only grows in size and never shrinks.
  ;;
  ;; (My guess is that the function takes (space :width text) at face
  ;; value, but that can’t be the whole picture because it works fine
  ;; when I manually evaluate the function in the childframe...)
  ;;
  ;; The original workaround of setting the frame size to something
  ;; small before calling ‘window-text-pixel-size’ works, but brings
  ;; other problems. Now we just set the display property to nil
  ;; before calling ‘window-text-pixel-size’, and set them back after.
  (setcdr eldoc-box--markdown-separator-display-props nil)

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

    ;; Set the display property back.
    (setcdr eldoc-box--markdown-separator-display-props
            '(:width text))

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

      (set-face-attribute 'fringe frame :background 'unspecified :inherit 'eldoc-box-body)
      (set-window-dedicated-p window t)
      (redirect-frame-focus frame (frame-parent frame))
      (set-face-attribute 'internal-border frame :inherit 'eldoc-box-border)
      (when (facep 'child-frame-border)
        (set-face-background 'child-frame-border
                             (face-attribute 'eldoc-box-border :background nil t)
                             frame))
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

(defun eldoc-box--count-newlines (str)
  "Count the number of newlines in STR, excluding invisible ones.
Trailing newlines doesn’t count."
  (let ((idx 0)
        (count 0)
        (last-visible-newline nil)
        (len (length str))
        ;; Is the last visible newline a trailing newline?
        (last-newline-trailing-p nil))

    ;; Count visible newlines in STR.
    (while (and (not (eq idx len))
                (setq idx (string-search "\n" str
                                         (if (eq idx 0) 0 (1+ idx)))))
      (unless (memq 'invisible (text-properties-at idx str))
        (setq last-visible-newline idx)
        (cl-incf count)))

    ;; If there is any visible character after the last newline, it is
    ;; not a trailing newline.
    (when last-visible-newline
      (setq last-newline-trailing-p t)
      (let ((idx (1+ last-visible-newline)))
        (while (< idx len)
          (when (not (memq 'invisible (text-properties-at idx str)))
            (setq last-newline-trailing-p nil))
          (cl-incf idx))))

    (if last-newline-trailing-p
        (1- count)
      count)))

(defun eldoc-box--eldoc-message-function (str &rest args)
  "Front-end for eldoc.
Display STR in childframe and ARGS works like `message'."
  (when (stringp str)
    (let* ((doc (string-trim-right (apply #'format str args)))
           (single-line-p (and eldoc-box-only-multi-line
                               (eq (eldoc-box--count-newlines doc) 0))))
      (when (and (not (equal doc ""))
                 (not single-line-p))
        (eldoc-box--display doc)
        (setq eldoc-box--last-point (point))
        ;; Why a timer? ElDoc is mainly used in minibuffer,
        ;; where the text is constantly being flushed by other commands
        ;; so ElDoc doesn't try very hard to cleanup
        (when eldoc-box--cleanup-timer
          (cancel-timer eldoc-box--cleanup-timer))
        ;; This function is also called by
        ;; `eldoc-pre-command-refresh-echo-area' in
        ;; `pre-command-hook', which means the timer is reset before
        ;; every command if `eldoc-box-hover-mode' is on and
        ;; `eldoc-last-message' is not nil.
        (setq eldoc-box--cleanup-timer
              (run-with-timer eldoc-box-cleanup-interval
                              nil #'eldoc-box--maybe-cleanup)))
      ;; Return nil to stop ‘eldoc--message’ from running, because
      ;; this function is added as a ‘:before-while’ advice.
      single-line-p)))

(defun eldoc-box--compose-doc (doc)
  "Compose a doc passed from eldoc.

DOC has the form of (TEXT :KEY VAL...), and KEY can be ‘:thing’
and ‘:face’, among other things. If ‘:thing’ exists, it is put at
the start of the doc followed by a colon. If ‘:face’ exists, it
is applied to the thing.

Return the composed string."
  (let ((thing (plist-get (cdr doc) :thing))
        (face (plist-get (cdr doc) :face)))
    (concat (if thing
                (concat (propertize (format "%s" thing) 'face face) ": ")
              "")
            (car doc))))

(defun eldoc-box--eldoc-display-function (docs interactive)
  "Display DOCS in childframe.
For DOCS and INTERACTIVE see ‘eldoc-display-functions’. Maybe
display the docs in echo area depending on
‘eldoc-box-only-multi-line’."
  (let ((doc (string-trim (string-join
                           (mapcar #'eldoc-box--compose-doc docs)
                           eldoc-box-doc-separator))))
    (when (eldoc-box--eldoc-message-function "%s" doc)
      (eldoc-display-in-echo-area docs interactive))))

;;;###autoload
(define-minor-mode eldoc-box-hover-mode
  "Display hover documentations in a childframe.
The default position of childframe is upper corner."
  :lighter eldoc-box-lighter
  (if eldoc-box-hover-mode
      (progn (when eldoc-box-hover-at-point-mode
               (eldoc-box-hover-at-point-mode -1))
             (eldoc-box--enable))
    (eldoc-box--disable)))

;;;###autoload
(define-minor-mode eldoc-box-hover-at-point-mode
  "A convenient minor mode to display doc at point.
You can use \\[keyboard-quit] to hide the doc."
  :lighter eldoc-box-lighter
  (if eldoc-box-hover-at-point-mode
      (progn (when eldoc-box-hover-mode
               (eldoc-box-hover-mode -1))
             (setq-local eldoc-box-position-function
                         eldoc-box-at-point-position-function)
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

(make-obsolete 'eldoc-box-eglot-help-at-point 'eldoc-box-help-at-point
               "v1.11.1")

(defun eldoc-box-eglot-help-at-point ()
  "Display documentation of the symbol at point.
This is now obsolete, you should use ‘eldoc-box-help-at-point’
instead."
  (interactive)
  (eldoc-box-help-at-point))

;;;; Company compatibility
;;

;; see also `eldoc-box--default-at-point-position-function'

;; please compiler
(defvar company-pseudo-tooltip-overlay)
(declare-function company-box--get-frame "company-box")

(defun eldoc-box--at-point-x-by-company ()
  "Return the x position that accommodates company's popup."
  (cond
   ((and (boundp 'company-pseudo-tooltip-overlay)
         company-pseudo-tooltip-overlay)
    (+ (* (frame-char-width)
          (+ (overlay-get company-pseudo-tooltip-overlay
                          'company-width)
             (overlay-get company-pseudo-tooltip-overlay
                          'company-column)))
       (or (line-number-display-width t) 0)))
   ((and (boundp 'company-box--x) (numberp company-box--x))
	(+ company-box--x
       (frame-pixel-width (company-box--get-frame))))
   (t nil)))

;;;; Markdown compatibility

(defvar-local eldoc-box--markdown-separator-display-props
    '(space :width text)
  "Stores the display text property applied to markdown separators.

Due to a bug, in ‘eldoc-box--update-childframe-geometry’, we
modify the display property temporarily and then set it back.")

(defun eldoc-box--prettify-markdown-separator ()
  "Prettify the markdown separator in doc returned by Eglot.
Refontify the separator so they span exactly the width of the
childframe."
  (save-excursion
    (goto-char (point-min))
    (let (prop)
      (while (setq prop (text-property-search-forward 'markdown-hr))
        (add-text-properties
         (prop-match-beginning prop)
         (prop-match-end prop)
         `( display ,eldoc-box--markdown-separator-display-props
            face eldoc-box-markdown-separator))))))

(defun eldoc-box--replace-en-space ()
  "Display the en spaces in documentation as regular spaces."
  (face-remap-set-base 'nobreak-space '(:inherit default))
  (face-remap-set-base 'markdown-line-break-face '(:inherit default)))

(defun eldoc-box--condense-large-newline-gaps ()
  "Condense exceedingly large gaps made of consecutive newlines.

These gaps are usually made of hidden \"```\" and/or consecutive
newlines. Replace those gaps with a single empty line at 0.5 line
height."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (rx (>= 2 (or "\n"
                          (seq bol "```" (* (syntax word)) "\n")
                          (seq (+ "<br>") "\n")
                          (seq bol (+ (or " " "\t" " ")) "\n"))))
            nil t)
      (if (or (eq (match-beginning 0) (point-min))
              (eq (match-end 0) (point-max)))
          (replace-match "")
        (replace-match "\n\n")
        (add-text-properties (1- (point)) (point)
                             '( font-lock-face (:height 0.4)
                                face (:height 0.4)))))))

(defun eldoc-box--remove-linked-images ()
  "Some documentation embed image links in the doc...remove them."
  (save-excursion
    (goto-char (point-min))
    ;; Find every Markdown image link, and remove them.
    (while (re-search-forward
            (rx "[" (seq "![" (+? anychar) "](" (+? anychar) ")") "]"
                "(" (+? anychar) ")")
            nil t)
      (replace-match ""))))

(defun eldoc-box--remove-noise-chars ()
  "Remove some noise characters like carriage return."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match ""))))

(defun eldoc-box--fontify-html ()
  "Fontify HTML tags and special entities."
  (save-excursion
    ;; <h1> tags.
    (goto-char (point-min))
    (while (re-search-forward
            (rx bol
                (group "<h" digit ">")
                (group (*? anychar))
                (group "</h" digit ">")
                eol)
            nil t)
      (add-text-properties (match-beginning 2)
                           (match-end 2)
                           '( face (:weight bold)
                              font-lock-face (:weight bold)))
      (put-text-property (match-beginning 1) (match-end 1)
                         'invisible t)
      (put-text-property (match-beginning 3) (match-end 3)
                         'invisible t))
    ;; Special entities.
    (goto-char (point-min))
    (while (re-search-forward (rx (or "&lt;" "&gt;" "&nbsp;")) nil t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'display
                         (pcase (match-string 0)
                           ("&lt;" "<")
                           ("&gt;" ">")
                           ("&nbsp;" " "))))))

;;;; Tab-bar compatibility

(defun eldoc-box-reset-frame ()
  "Discard the current childframe and regenerate one.
This allows any change in childframe parameter to take effect."
  (interactive)
  (setq eldoc-box--frame nil))

(with-eval-after-load 'tab-bar
  (add-hook 'tab-bar-mode-hook #'eldoc-box-reset-frame))

(with-eval-after-load 'tab-line
  (add-hook 'tab-line-mode-hook #'eldoc-box-reset-frame))

(provide 'eldoc-box)

;;; eldoc-box.el ends here
