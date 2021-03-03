;;; centered-cursor-mode.el --- Cursor stays vertically centered -*- lexical-binding: nil; -*-

;; Copyright (C) 2007  André Riemann

;; Author: André Riemann <andre.riemann@web.de>
;; Maintainer: André Riemann <andre.riemann@web.de>
;; Created: 2007-09-14
;; Keywords: convenience

;; URL: https://github.com/andre-r/centered-cursor-mode.el/
;; Compatibility: tested with GNU Emacs 28
;; Version: 0.7-SNAPSHOT
;; Package-Requires: ((emacs "25.1") seq)
;; Last-Updated: 2020-TODO

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Todo list: https://github.com/andre-r/centered-cursor-mode.el/blob/master/TODO.adoc

;;; Commentary:

;; Makes the cursor stay vertically in place in a window. Typically centered, but
;; other positions are possible like golden ratio. Instead of the cursor moving up
;; and down the buffer scrolls, giving the feeling like a pager and always having a
;; context around the cursor.
;;
;; The vertical position can not only customised but also instantly adjusted (see
;; `centered-cursor-bindings').

;; To load put that in .emacs:
;;     (require 'centered-cursor-mode)
;; To activate do:
;;     M-x centered-cursor-mode
;; for buffer local or
;;     M-x global-centered-cursor-mode
;; for global minor mode.
;; Also possible: put that in .emacs
;;     (and
;;      (require 'centered-cursor-mode)
;;      (global-centered-cursor-mode +1))
;; to always have centered-cursor-mode on in all buffers.
;;
;; TODO:
;; use-package:
;; (use-package ...)
;;
;; TODO:
;; Alternatives:
;; -scroll-preserve-screen-position: only works when scrolling, i.e. when point
;;     leaving window (pgup, pgdn)
;; - scroll-margin, maximum-scroll-margin: maximum is max. 0.5 and leaves 2-3 lines
;;     tolerance
;; - scroll-lock-mode
;; - (setq maximum-scroll-margin 0.5
;;         scroll-margin 99999
;;         scroll-preserve-screen-position t
;;         scroll-conservatively 0)
;; - ...

;;; Change Log:
;; 2020-TODO andre-r
;;   * refactored and simplified
;;   * simplier and more robust customisations
;;     * nicer customisation widgets and validation
;;     * no more ccm-recenter-at-end-of-file to inhibit recentering at the end of
;;       file, as it is a performance problem; maybe another solution is possible
;;   * no animation after suspended recentering, line is now highlighted
;;     (animation can be done again by function variable centered-cursor--jump-recenter-function)
;;
;; 2020-05-07 hlissner
;;   * autoload global-centered-cursor-mode
;; 2019-03-06 kqr
;;   * more customisable way to inhibit recentering after a command:
;;     new defcustom ccm-inhibit-centering-when
;;   * new ignored command evil-mouse-drag-region
;; 2019-02-24 Gollum999
;;   * Fix aggressive centering while dragging mouse (selecting text doesn't scroll)
;; 2019-02-05 andre-r
;;   * tip from MATTHIAS Andreas
;;     - replaced forward-line with next-line in ccm-scroll-up and ccm-scroll-down;
;;       scrolled too far in visual-line-mode
;; 2018-01-12 andre-r
;;   * #3: Centering does not take line-height into account
;;     - added new function for calculating visible lines
;;   * #2: Bug with collapsed lines (eg. org-mode)
;;     - used count-screen-lines instead of count-lines
;; 2017-08-30 chrm
;;   * Fixed a bug with recentering at end of file
;; 2015-10-01 Hinrik Örn Sigurðsson <hinrik.sig@gmail.com>
;;   * Avoided calling count-lines when unnecessary, which
;;     fixes slow scrolling in large files
;; 2015-03-01  andre-r
;;   * fixed bug where Emacs without X support (emacs-nox) didn't find mouse-wheel-mode
;; 2009-08-31  andre-r
;;   * replaced window-body-height with window-text-height
;;     (partially visible lines are not counted in window-text-height)
;;   * bug fixed in ccm-vpos-recenter
;;     (some parentheses where wrong after the last update)
;; 2009-02-23  andre-r
;;   * some simplifications
;; 2009-02-22  andre-r
;;   * some tips from Drew Adams:
;;     - new local variable coding:utf-8
;;     - made recenter-sequence a defvar
;;     - added groups scrolling and convenience
;;     - replaced mouse-4 and mouse-5 with
;;       mouse-wheel-up-event and mouse-wheel-down-event
;;     - added scroll-bar-toolkit-scroll to ccm-ignored-commands
;;     - made ccm-ignored-commands customisable
;;   * removed a bug where it didn't work with more than one window
;;     displaying the same buffer
;;   * added function for page up and down scrolling
;;     (standard ones didn't work well with this mode)
;;   * made the animation delay customisable
;;   * made the initial vertical position customisable
;;   * made the behaviour at the end of the file customisable
;; 2008-02-02  andre-r
;;   * fixed bug that led to wrong-type-argument
;;     when opening a new buffer
;;   * some other minor stuff
;; 2007-09-24  andre-r
;;   * added global minor mode
;; 2007-09-21  andre-r
;;   * not recentering at end of buffer
;;   * defvar animate-first-start-p
;; 2007-09-14  andre-r
;;   * inital release

;; This file is *NOT* part of GNU Emacs.

;;; Code:

(defconst centered-cursor--log-p t) ; TODO

(eval-when-compile
  (require 'mouse-wheel-mode nil 'noerror)
  (require 'pulse nil 'noerror)
  (require 'seq)) ; TODO wenn emacs < 24.1

(make-obsolete-variable 'ccm-step-size "Animation was replaced by line highlighting" "0.7")
(make-obsolete-variable 'ccm-step-delay "Animation was replaced by line highlighting" "0.7")
(make-obsolete-variable 'ccm-vpos-init "Replaced by new variable centered-cursor-position, but it has another structure" "0.7")
(make-obsolete-variable 'ccm-recenter-at-end-of-file "Defcustom removed for performance reasons" "0.7")
(make-obsolete-variable 'ccm-inhibit-centering-when nil "0.7") ; was defcustom, TODO alias possible?
(define-obsolete-variable-alias 'ccm-ignored-commands 'centered-cursor-ignored-commands "0.7")
(define-obsolete-function-alias 'ccm-ignored-command-p 'centered-cursor--ignored-command-p "0.7")
(define-obsolete-function-alias 'ccm-mouse-drag-movement-p 'centered-cursor--mouse-drag-movement-p "0.7")
(define-obsolete-variable-alias 'ccm-keymap 'centered-cursor-keymap "0.7")
(define-obsolete-function-alias 'ccm-scroll-up 'centered-cursor-scroll-up "0.7")
(define-obsolete-function-alias 'ccm-scroll-down 'centered-cursor-scroll-down "0.7")

;;; Customisation

(defgroup centered-cursor nil
  "Makes the cursor stay vertically in a defined position (usually centered).
Instead the cursor the text moves around the cursor."
  :group 'scrolling
  :group 'convenience
  :link '(emacs-library-link :tag "Source Lisp File" "centered-cursor-mode.el")
  :link '(url-link "https://github.com/andre-r/centered-cursor-mode.el"))

(defcustom centered-cursor-lighter " ¢-DEV"
  "Lighter for mode line."
  :group 'centered-cursor
  :tag "Mode line symbol"
  :type '(choice (string :tag "Custom string" :format "%{%t%}: %v" :size 10)
                 (const :tag "None" :value "")))

;; not inherited from 'function, because allowing commands that are unknown
;; at the moment
(define-widget 'centered-cursor--command-widget 'symbol
  "A command."
  :tag "Command"
  ;; completion for commands (but other symbols are allowed)
  :completions (apply-partially #'completion-table-with-predicate
                                obarray #'commandp 'strict)
  :match-alternatives '(commandp)) ;  :match-alternatives necessary? adopted and
                                   ;  adjusted from function widget

(defcustom centered-cursor-ignored-commands '(mouse-drag-region
                               mouse-set-region
                               mouse-set-point
                               widget-button-click
                               scroll-bar-toolkit-scroll
                               evil-mouse-drag-region
                               handle-select-window
                               tabbar-select-tab-callback)
  "After these commands recentering is ignored.
This is to prevent unintentional jumping (especially when mouse
clicking). Following commands (except the ignored ones) will
cause an animated recentering to give a feedback and not just
jumping to the center."
  :group 'centered-cursor
  :tag "Ignored commands"
  :type '(repeat centered-cursor--command-widget))

(defcustom centered-cursor-position 'centered
  "This is the screen line position where the cursor initially stays."
  :group 'centered-cursor
  :tag "Vertical cursor position"
  :type '(choice (const :tag "Centered" centered)
                 (const :tag "Golden ratio (cursor in lower half)" golden-ratio)
                 ;; const e.g. ... '(centered-cursor-position 'golden-ratio) ...
                 (const :tag "Golden ratio (cursor in upper half)" golden-ratio-from-bottom)
                 ;; cons e.g. ... '(centered-cursor-position '(ratio . 0.4)) ...
                 (cons :tag "Ratio"
                       :format "%t: %v\n"
                       (const :format "" ratio)
                       (float :format "%v"
                              :value 0.5
                              :size 7
                              :validate (lambda (widget)
                                          (let ((value (widget-value widget)))
                                            (when (or (< value 0.0)
                                                      (> value 1.0))
                                              (widget-put widget :error (format "Ratio must be between (including) 0.0 and 1.0: %S" value))
                                              widget)))))
                 (cons :tag "Lines from top"
                       :format "%t: %v\n"
                       (const :format "" lines-from-top)
                       (integer :format "%v"
                                :value 10
                                :size 5
                                :validate (lambda (widget)
                                            (let ((value (widget-value widget)))
                                              (when (< value 1)
                                                (widget-put widget :error (format "Value must be greater than 0: %S" value))
                                                widget)))))
                 (cons :tag "Lines from bottom"
                       :format "%t: %v\n"
                       (const :format "" lines-from-bottom)
                       (integer :format "%v"
                                :value 10
                                :size 5
                                :validate (lambda (widget)
                                            (let ((value (widget-value widget)))
                                              (when (< value 1)
                                                (widget-put widget :error (format "Value must be greater than 0: %S" value))
                                                widget)))))
                 (cons :tag "Custom function"
                       :format "%t: %v"
                       (const :format "" custom-function)
                       (function :format "%v"))))
(make-variable-buffer-local 'centered-cursor-position)

;;; Variables

(defvar-local centered-cursor--old-point nil "Point before a command for comparison purposes.")
(defvar-local centered-cursor--calculated-position nil "Calculated line as argument for `recenter'.")
(defvar-local centered-cursor--manual-position nil "Manually set line as argument -- if set -- for `recenter'.")

(defvar centered-cursor--inhibit-centering-when '(centered-cursor--ignored-command-p
                                   centered-cursor--mouse-drag-movement-p)
  "A list of functions which are allowed to inhibit recentering.
If any of these return t, recentering is cancelled.")

(defun centered-cursor--ignored-command-p ()
  "Check if the last command was one listed in `centered-cursor-ignored-commands'."
  (when (member this-command centered-cursor-ignored-commands)
    (centered-cursor--log "ignored: %s" this-command)
    t))

(defun centered-cursor--mouse-drag-movement-p ()
  "Check if the last input event corresponded to a mouse drag event."
  (when (mouse-movement-p last-command-event)
    (centered-cursor--log "ignored mouse")
    t))

;;; Keymap and keys

(defvar centered-cursor-keymap (make-sparse-keymap) "The keymap for Centered-Cursor mode.")

(defvar centered-cursor-bindings
  '((define-key centered-cursor-keymap (kbd "C-M--") 'centered-cursor-raise-position-manually)
    (define-key centered-cursor-keymap (kbd "C-M-+") 'centered-cursor-lower-position-manually)
    (define-key centered-cursor-keymap (kbd "C-M-=") 'centered-cursor-lower-position-manually)
    (define-key centered-cursor-keymap (kbd "C-M-0") 'centered-cursor-reset-position-manually))
  "List of binding forms evaluated by command centered-cursor-bindings'.
Default bindings can be adjusted with own bindings. This has to
be done before calling command `centered-cursor-bindings'.")

;;;###autoload
(defun centered-cursor-bindings ()
  "Evaluate the forms in variable `centered-cursor-bindings'.
Called to apply default key bindings."
  (interactive)
  (eval (cons 'progn centered-cursor-bindings)))

;;; Setting position manually

;;;###autoload
(defun centered-cursor-lower-position-manually (arg)
  "Move the screen position of the cursor downwards by ARG lines.
Negative values for ARG are possible. Internally the variable
`centered-cursor--manual-position' gets a new value.
See `centered-cursor-raise-position-manually'."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((new-pos (if (< centered-cursor--calculated-position 0)
                     (- centered-cursor--calculated-position arg)
                   (+ centered-cursor--calculated-position arg)))
        ;; see pos-visible-in-window-p
        (vpos-max (if (< centered-cursor--calculated-position 0)
                      -1
                    (- (centered-cursor--screen-lines) 1)))
        (vpos-min (if (< centered-cursor--calculated-position 0)
                      (- (centered-cursor--screen-lines))
                    0)))
    (setq centered-cursor--manual-position
          (cond
           ((< new-pos vpos-min)
            vpos-min)
           ((> new-pos vpos-max)
            vpos-max)
           (t
            new-pos)))
    (setq centered-cursor--calculated-position centered-cursor--manual-position)))

;;;###autoload
(defun centered-cursor-raise-position-manually (arg)
  "Move the screen position of the cursor upwards by ARG lines.
Negative values for ARG are possible. Internally the variable
`centered-cursor--manual-position' gets a new value.
See `centered-cursor-lower-position-manually'."
  (interactive "p")
  (or arg (setq arg 1))
  (centered-cursor-lower-position-manually (- arg)))

;;;###autoload
(defun centered-cursor-reset-position-manually ()
  "Reset the manually set screen position of the cursor.
The customised position in `centered-cursor-position' is then
used again for recentering."
  (interactive)
  (setq centered-cursor--manual-position nil)
  (centered-cursor-calculate-position))

;;; Scrolling/cursor movement

(defun centered-cursor--default-scroll-amount ()
  "Default scroll amount for `centered-cursor--scroll-command' is a near full screen.
It is calulated by `centered-cursor--screen-lines' minus `next-screen-context-lines'.
See also `scroll-down-command'."
  (- (centered-cursor--screen-lines) next-screen-context-lines))

(defun centered-cursor--scroll-command (arg direction)
  "Internal function for scrolling up or down.
Scroll ARG lines, direction depending on PREFIX-FUNC. Used by
`centered-cursor-scroll-up' and `centered-cursor-scroll-down' for
page up or down and mouse wheel. Uses `line-move'."
  (line-move (cond
              ((null arg)
               (* direction (centered-cursor--default-scroll-amount)))
              ((eq arg #'-)
               (* (- direction) (centered-cursor--default-scroll-amount)))
              (t
               (* direction arg)))))

;;;###autoload
(defun centered-cursor-scroll-up (&optional arg)
  "Replacement for `scroll-up'.
Instead of scrolling, the cursor if moved down linewise by ARG."
  (interactive "^P")
  (centered-cursor--scroll-command arg 1))

;;;###autoload
(defun centered-cursor-scroll-down (&optional arg)
  "Replacement for `scroll-down'.
Instead of scrolling, the cursor is moved up linewise by ARG."
  (interactive "^P")
  (centered-cursor--scroll-command arg -1))

;;; Recentering

(defvar centered-cursor--jump-recenter-function #'centered-cursor--highlight-recenter
  "Function that does recentering after recentering was
  suspended, e.g. after setting point with the mouse.")

(defun centered-cursor--highlight-recenter ()
  "TODO"
  (recenter centered-cursor--calculated-position)
  (pulse-momentary-highlight-one-line (point)))

(defun centered-cursor-calculate-position ()
  "Calculate and set the vertical cursor position.
The cursor position -- the screen line -- is calculated according
to the customisation in `centered-cursor-position'."
  (interactive)
  (when (equal (current-buffer)
               (window-buffer (selected-window)))
    (setq centered-cursor--calculated-position
          ;; if position is set manually, don't calculate and use this
          (if centered-cursor--manual-position
              centered-cursor--manual-position
            (condition-case ex ; exception handling
                (let ((position centered-cursor-position)
                      (height (centered-cursor--screen-lines)))
                  (if (consp position) ; e.g. (ratio . 0.4)
                      (let ((key (car position))
                            (value (cdr position)))
                        (case key
                          ('ratio
                           (round (* height
                                     (centered-cursor--constrain value 0.0 1.0))))
                          ('lines-from-top
                           (centered-cursor--constrain (1- value) 0 (1- height)))
                          ('lines-from-bottom
                           (- height (centered-cursor--constrain value 0 height)))
                          ('custom-function
                           (setq value (funcall value))
                           (when (not (numberp value))
                             (signal 'wrong-type-argument value))
                           (if (integerp value)
                               (centered-cursor--constrain value 0 height)
                             (centered-cursor--constrain value 0.0 1.0)))))
                    ;; else position not cons but const
                    (cond
                     ((eq position 'centered)
                      (round height 2))
                     ((eq position 'golden-ratio)
                      (round (* 21 height) 34))
                     ((eq position 'golden-ratio-from-bottom)
                      (- height (round (* 21 height) 34))))))
              (error (progn
                       ;; return default value "centered" if customised value is faulty
                       (message "Error in `centered-cursor-position'. Defaulting to `centered'")
                       (round height 2))))))
    (if (= centered-cursor--calculated-position (centered-cursor--screen-lines))
        ;; It is possible that window-text-height counts a partially visible bottom
        ;; -- TODO
        ;; line. Docstring says, it does not, but there where situations where this
        ;; happened (default-text-scale-mode?). Depending on customisation, the
        ;; cursor then jumps centered to not be in a partially visible line.
        ;; -> correct to bottom line as negative number
        -1
      centered-cursor--calculated-position)))

(defun centered-cursor--constrain (value min max)
  "Return VALUE if between MIN and MAX.
Return MAX if VALUE is greater than MAX.
Return MIN if VALUE is less than MIN."
  (max min (min max value)))

;;;###autoload
(defun centered-cursor-recenter (&optional first-start-p)
  "TODO"
  ;; (condition-case ex ; exception handling
  (when centered-cursor-mode
    (if (centered-cursor--inhibit-centering-p)
        (progn
          (centered-cursor--log-top-values)
          (centered-cursor--log "ignored %s" this-command))
      (centered-cursor--do-recenter first-start-p)))
  ;; (error (prog1 nil
  ;; (centered-cursor-mode 0)
  ;; (message "Centered-Cursor mode disabled in buffer %s due to error: %s"
  ;; (buffer-name) ex)))))
  )
(defun centered-cursor--inhibit-centering-p ()
  (seq-some #'funcall centered-cursor--inhibit-centering-when))

(defun centered-cursor--do-recenter (&optional first-start-p)
  "TODO"
  (when (equal (current-buffer) (window-buffer (selected-window))) ;; TODO or mouse scrolls (doesn't have to be current buffer)
    ;; (redisplay) ; flickers, but only after redisplay window-end is current, see centered-cursor--log
    (centered-cursor--log-top-values)
    ;; (centered-cursor--log "--do-recenter")
    (unless centered-cursor--calculated-position
      (centered-cursor-calculate-position))
    ;; only animate if the point was moved rather far away
    ;; before by a mouseclick (see centered-cursor-ignored-commands)
    ;; or if minor mode is just entered interactively
    (if (or first-start-p
            (and (member last-command centered-cursor-ignored-commands)
                 (> (centered-cursor--visual-line-diff centered-cursor--old-point (point)) 3)))
        (funcall centered-cursor--jump-recenter-function)
      (recenter centered-cursor--calculated-position))
    (setq centered-cursor--old-point (point))))

(defun centered-cursor--visual-line-diff (start end)
  (save-restriction
    (save-excursion
      (goto-char start)
      (vertical-motion 0)
      (setq start (point))
      (goto-char end)
      (vertical-motion 0)
      (setq end (point))
      (count-screen-lines start end))))

(defun centered-cursor--screen-lines ()
  (floor (window-screen-lines)))

;;; Overriding functions and compatibility

(define-key centered-cursor-keymap [remap scroll-down-command] 'centered-cursor-scroll-down)
(define-key centered-cursor-keymap [remap scroll-up-command] 'centered-cursor-scroll-up)
(define-key centered-cursor-keymap [remap scroll-bar-scroll-down] 'centered-cursor-scroll-down)
(define-key centered-cursor-keymap [remap scroll-bar-scroll-up] 'centered-cursor-scroll-up)

(defun centered-cursor-View-scroll-page-backward (&optional lines)
  "Scroll down LINES lines.
Replaces `View-scroll-page-backward' in Centered-Cursor mode for
compatibility."
  (interactive "P")
  (let ((lines (or lines (view-page-size-default view-page-size))))
    (centered-cursor-scroll-down lines)))
(define-key centered-cursor-keymap [remap View-scroll-page-backward] 'centered-cursor-View-scroll-page-backward)

(defun centered-cursor-View-scroll-page-forward (&optional lines)
  "Scroll up LINES lines.
Replaces `View-scroll-page-forward' in Centered-Cursor mode for
compatibility."
  (interactive "P")
  (let ((lines (or lines (view-page-size-default view-page-size))))
    (centered-cursor-scroll-up lines)))
(define-key centered-cursor-keymap [remap View-scroll-page-forward] 'centered-cursor-View-scroll-page-forward)

;;

(defun centered-cursor-replace-scroll-down--around (orig-fun &optional args)
  (cl-letf (((symbol-function 'scroll-down) 'centered-cursor-scroll-down))
    (condition-case ex
        (funcall orig-fun args)
      (error (centered-cursor-scroll-down)))))
(advice-add 'Info-scroll-down :around #'centered-cursor-replace-scroll-down--around)
(advice-add 'evil-scroll-page-up :around #'centered-cursor-replace-scroll-down--around)
;; (advice-add 'scroll-bar-toolkit-scroll :around #'centered-cursor-replace-scroll-down--around)

(defun centered-cursor-replace-scroll-up--around (orig-fun &optional args)
  (cl-letf (((symbol-function 'scroll-up) 'centered-cursor-scroll-up))
    (condition-case ex
        (funcall orig-fun args)
      (error (centered-cursor-scroll-up))))) ;; error see centered-cursor-Info-scroll-down--around
(advice-add 'Info-scroll-up :around #'centered-cursor-replace-scroll-up--around)
(advice-add 'evil-scroll-page-down :around #'centered-cursor-replace-scroll-up--around)
;; (advice-add 'scroll-bar-toolkit-scroll :around #'centered-cursor-replace-scroll-up--around) ;; doesn't work completely


;; (defun centered-cursor-Info-scroll-down--around (orig-fun)
;; "Around advice for `Info-scroll-down'.
;; Make `Info-scroll-down' -- called by ORIG-FUN -- use
;; `centered-cursor-scroll-down' instead of `scroll-down'. Error
;; handling is still necessary to assure going back a node works."
;; (cl-letf (((symbol-function 'scroll-down) 'centered-cursor-scroll-down))
;; (condition-case ex
;; (funcall orig-fun)
;; (user-error (centered-cursor-scroll-down)))))
;; (advice-add 'Info-scroll-down :around #'centered-cursor-Info-scroll-down--around)

;; (defun centered-cursor-Info-scroll-up--around (orig-fun)
;; "Around advice for `Info-scroll-up'.
;; Make `Info-scroll-up' -- called by ORIG-FUN -- use
;; `centered-cursor-scroll-up' instead of `scroll-up'."
;; (cl-letf (((symbol-function 'scroll-up) 'centered-cursor-scroll-up))
;; (funcall orig-fun)))
;; (advice-add 'Info-scroll-up :around #'centered-cursor-Info-scroll-up--around)

;;

(when (boundp mouse-wheel-mode)
  (defvar-local centered-cursor--original-mwheel-scroll-up-function 'scroll-up)
  (defvar-local centered-cursor--original-mwheel-scroll-down-function 'scroll-down)

  (defun centered-cursor--set-mwheel-scroll-functions ()
    "Set variables that do the scrolling in package `mwheel.el'.
`mwheel-scroll-up-function' and `mwheel-scroll-down-function' are
  set to `next-line' and `previous-line' respectively."
    (setq centered-cursor--original-mwheel-scroll-up-function
          mwheel-scroll-up-function)
    (setq centered-cursor--original-mwheel-scroll-down-function
          mwheel-scroll-down-function)
    (setq mwheel-scroll-up-function 'next-line)
    (setq mwheel-scroll-down-function 'previous-line))

  (defun centered-cursor--reset-mwheel-scroll-functions ()
    "Reset variables to original that do the scrolling in package `mwheel.el'.
Previously set by `centered-cursor--set-mwheel-scroll-functions'."
    (setq mwheel-scroll-up-function
          centered-cursor--original-mwheel-scroll-up-function)
    (setq mwheel-scroll-down-function
          centered-cursor--original-mwheel-scroll-down-function)))




;; Testing:

;; doesn't work
;; (defvar-local scroll-up (symbol-function 'centered-cursor-scroll-up))
;; (defvar-local scroll-down (symbol-function 'centered-cursor-scroll-down))

;;; Hooks

(defvar centered-cursor--hook-alist
  '((post-command-hook . centered-cursor--post-command-hook)
    ;; (after-change-functions . centered-cursor--after-change-function)
    (window-configuration-change-hook . centered-cursor--window-configuration-change-hook)
    ;; (window-scroll-functions . centered-cursor--window-scroll-function)
    (text-scale-mode-hook . centered-cursor--window-configuration-change-hook))
  "A list of hooks.
List of cons cells in format (hook-variable . function).")

(defun centered-cursor--add-hooks ()
  "Add hooks defined in variable `centered-cursor-hook-alist'."
  (mapc (lambda (entry)
          (add-hook (car entry) (cdr entry) t t))
        centered-cursor--hook-alist))

(defun centered-cursor--remove-hooks ()
  "Remove hooks defined in variable `centered-cursor-hook-alist'."
  (mapc (lambda (entry)
          (remove-hook (car entry) (cdr entry) t))
        centered-cursor--hook-alist))

(defun centered-cursor-mode-unload-function ()
  "Cancel all Centered-Cursor modes in buffers.
Called by function `unload-feature'."
  (centered-cursor--log "--mode-unload-function")
  (global-centered-cursor-mode 0))

(defun centered-cursor--post-command-hook ()
  "Called after every command."
  ;; (while-no-input (centered-cursor-recenter)))
  (centered-cursor-recenter))

(defun centered-cursor--window-configuration-change-hook ()
  "Called after resizing a window and after mode start.
After resizing a window the position has to be recalculated."
  (centered-cursor-calculate-position)
  (centered-cursor-recenter))

;;; Logging

(defconst centered-cursor--log-buffer-name "*centered-cursor-log*")

(defun centered-cursor--log-top (string &rest objects)
  "Internal log function for logging variables.
STRING and OBJECTS are formatted by `format'. Makes sure a page
break (^L) is inserted after. Logged events are logged below page
break by function `centered-cursor--log'. See
`centered-cursor--log-top-values' for values logged on top."
  (when centered-cursor--log-p
    (let ((log-buffer (or (get-buffer centered-cursor--log-buffer-name)
                          (generate-new-buffer centered-cursor--log-buffer-name))))
      (with-current-buffer log-buffer
        (goto-char (point-min))
        (delete-region (point-min)
                       (progn
                         (search-forward-regexp (concat "^" (char-to-string ?\^L) "$") nil t)
                         (vertical-motion 1)
                         (point)))
        (insert (apply #'format
                       (concat string (char-to-string ?\^L) "\n")
                       objects))))))

(defun centered-cursor--log (string &rest objects)
  "Internal log function for logging messages.
STRING and OBJECTS are formatted by `format'."
  (when centered-cursor--log-p
    (let ((log-buffer
           (let ((name "*centered-cursor-log*"))
             (or (get-buffer name)
                 (generate-new-buffer name))))
          (buffer (buffer-name))
          (max-log-lines 30)
          (message (apply #'format (concat string "\n") objects))
          hlinepos
          hlineline)
      (with-current-buffer log-buffer
        (goto-char (point-min))
        (setq hlinepos
              ;; horizontal line (^L) inserted by --log-top
              (or (search-forward (concat (char-to-string ?\^L) "\n") nil t)
                  ;; or no line
                  (point-min)))
        (setq hlineline (line-number-at-pos hlinepos))
        (while (> (count-lines (point-min) (point-max))
                  (+ hlineline max-log-lines))
          (goto-char hlinepos)
          (delete-region (line-beginning-position 1) (line-beginning-position 2)))
        (goto-char (point-max))
        (insert (concat (format-time-string "%F %T") " [" buffer "] " message))))))

(defun centered-cursor--log-top-values ()
  (centered-cursor--log-top
   "Values before recentering:
==========================

last-command-event:  %s
---> mouse-event-p:  %s
this-command:        %s
last-command:        %s
visual-text-lines:   %s
centered-cursor-position: %s
delta: %s
window-end: %s (only up-to-date after redisplay, after recentering!)
point-max:  %s
"
   last-command-event
   (mouse-event-p last-command-event)
   this-command
   last-command

   (centered-cursor--screen-lines)

   centered-cursor--calculated-position
   (centered-cursor--visual-line-diff centered-cursor--old-point (point))

   (window-end)
   (point-max)))

;;; Mode definition and start

(defun centered-cursor-turn-on ()
  "Try to turn on Centered-Cursor mode.
Called when calling command `global-centered-cursor-mode'.
Centered-Cursor mode will not start in minibuffer,
*centered-cursor-log* (defined in variable
`centered-cursor--log-buffer-name') and hidden buffers."
  ;; ignore mode in minibuffer, *centered-cursor-log* buffer or invisible buffers
  (unless (or (minibufferp)
              (string-equal centered-cursor--log-buffer-name (buffer-name))
              (string-match-p (rx string-start " *" (+? anything) "*")
                              (buffer-name)))
    (centered-cursor-mode 1)))

(defun centered-cursor--first-start ()
  "Executed when starting Centered-Cursor mode.
Recenters initially and -- in the current buffer -- highlights
current line."
  (setq centered-cursor--old-point (point))
  (centered-cursor-calculate-position)
  (centered-cursor-recenter t))

;;;###autoload
(define-minor-mode centered-cursor-mode
  "Makes the cursor stay vertically in place in a window.
Typically centered, but other positions are possible like golden
ratio. Instead of the cursor moving up and down the buffer
scrolls, giving the feeling like a pager and always having a
context around the cursor.

The vertical position can not only be customised but also
instantly adjusted (see key bindings below).

Key bindings:
\\{centered-cursor-keymap}"
  :init-value nil
  :lighter centered-cursor-lighter
  :keymap centered-cursor-keymap
  (cond
   (centered-cursor-mode
    (centered-cursor--first-start)
    (centered-cursor--add-hooks)
    (centered-cursor--set-mwheel-scroll-functions)
    (centered-cursor--log "Centered-Cursor mode enabled"))
   (t
    (centered-cursor--remove-hooks)
    (centered-cursor--reset-mwheel-scroll-functions)
    (centered-cursor--log "Centered-Cursor mode disabled"))))

;;;###autoload
(define-globalized-minor-mode global-centered-cursor-mode centered-cursor-mode
  centered-cursor-turn-on)

(provide 'centered-cursor-mode)

;; Local Variables:
;; eval: (when (boundp 'origami-mode) (origami-mode))
;; coding: utf-8
;; nameless-current-name: "centered-cursor"
;; nameless-prefix: "@"
;; End:

;;; centered-cursor-mode.el ends here
