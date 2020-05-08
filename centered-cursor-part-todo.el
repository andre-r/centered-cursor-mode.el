;;
;;
;;testline for visual line mode ##############################################################################################################################################################


(setq centered-cursor--log-p t)

(require 'mouse-wheel-mode nil 'noerror)

(require 'ccm-dev)

;;; Customization


(defvar-local ccm--old-point nil)

;; not used?
(defvar-local ccm--total-screen-lines-l nil)

(defun centered-cursor--total-screen-lines ()
  "Displayed lines in the whole buffer including the final newline."
  (when (not ccm--total-screen-lines-l)
    (setq ccm--total-screen-lines-l
          (count-screen-lines (point-min) (point-max) t))
    ;; (centered-cursor--log "total screen lines: %s" ccm--total-screen-lines-l)
    )
  ccm--total-screen-lines-l)

(defun centered-cursor--reset-total-screen-lines-l ()
  (setq ccm--total-screen-lines-l nil))


;; (defvar-local ccm-pos nil)
;; (make-obsolete-variable 'ccm-pos nil "0.7")
(defvar-local centered-cursor-position nil)

;;; keymap and keys

;;; Overriding functions and compatibility

;;; position manipulating

;;;###autoload
(defun ccm-set-pos-down (arg)
  (interactive "p")
  (or arg (setq arg 1))
  (let ((new-pos (if (< centered-cursor-position 0)
                     (- centered-cursor-position arg)
                   (+ centered-cursor-position arg)))
        ;; see pos-visible-in-window-p
        (vpos-max (if (< centered-cursor-position 0)
                      -1
                    (- (window-text-height) 1)))
        (vpos-min (if (< centered-cursor-position 0)
                      (- (window-text-height))
                    0)))
    (setq centered-cursor-position
          (cond
           ((< new-pos vpos-min)
            vpos-min)
           ((> new-pos vpos-max)
            vpos-max)
           (t
            new-pos)))))

;;;###autoload
(defun ccm-set-pos-up (arg)
  (interactive "p")
  (or arg (setq arg 1))
  (ccm-set-pos-down (- arg)))

;;;###autoload
(defun centered-cursor-position-cursor (&optional interactive-p)
  (condition-case ex ; exception handling
	  (when (or centered-cursor-mode
				global-centered-cursor-mode)
		(if (or (member this-command centered-cursor-ignored-commands)
                (mouse-event-p last-command-event))
            (progn
              (centered-cursor--log-top-values)
              (centered-cursor--log "ignored %s" this-command))
		  (centered-cursor--refresh interactive-p)))
	(error (prog1 nil
			 (global-centered-cursor-mode 0)
			 (message "Centered-Cursor mode disabled due to error: %s" ex)))))

(defun centered-cursor--refresh (&optional interactive-p)
  (if (and (eq centered-cursor-initial-position 'centered)
           centered-cursor-recenter-at-end-of-buffer)
      ;; experimental fast way
      (progn (centered-cursor--log-top-values)
             (centered-cursor--log "--refresh faster")
             (recenter nil nil))
    (unless centered-cursor-position
	  (centered-cursor-calculate-position))
    (when (and (not (minibufferp (current-buffer)))
			   (equal (current-buffer) (window-buffer (selected-window))))
      (centered-cursor--log-top-values)
      (centered-cursor--log "--refresh")
	  ;; only animate if the point was moved rather far away
	  ;; before by a mouseclick (see ccm-ignored-commands)
	  ;; or if minor mode is just entered interactively
	  (if (or (and (member last-command centered-cursor-ignored-commands) ;; TODO im moment muss
                   ;; last-command in ignored-commands sein, weil
                   (let ((delta (centered-cursor--visual-line-diff ccm--old-point (point))))
                     (> delta 4)))
              interactive-p)
          (centered-cursor--jump-recenter)
        (ccm--recenter))
      (setq ccm--old-point (point)))))


(defun centered-cursor--visual-line-number-at-pos (&optional pos)
  (let ((start (point-min))
        (end (or pos (point))))
    (1+ (centered-cursor--visual-line-diff start end))))

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

(defvar-local centered-cursor--old-visual-pos (centered-cursor--visual-line-number-at-pos))

(defun centered-cursor--next-recenter-function-arg (pos)
  (if (< pos 0)
	  ;; one-based, from bottom, negative
	  (- (count-screen-lines
		  (point)
		  (max (window-end) (point)) t)) ; window-end is sometimes negative when opening a help buffer
	;; zero-based, from top, positive
	(1- (count-screen-lines (window-start)
		                    (point) t))))

(defun ccm--recenter ()
  ;; if near the bottom, recenter in the negative screen line that equals the bottom
  ;; buffer line, i.e. if we are in the second last line (-2) of the buffer, the
  ;; cursor will be recentered in -2
  (recenter (if ccm-recenter-at-end-of-file
                centered-cursor-position
              (let ((lines-to-eob (- (centered-cursor--total-screen-lines)
                                     (centered-cursor--visual-line-number-at-pos (point))))
                    (lines-to-eow (- (window-text-height) centered-cursor-position)))
                (if (<= lines-to-eob lines-to-eow)
                    (- (1+ lines-to-eob))
                  centered-cursor-position)))))

(defun centered-cursor--first-start (&optional interactive-p)
  (setq ccm--old-point (point))
  (centered-cursor-calculate-position)
  (centered-cursor-position-cursor interactive-p))


;;; Logging

;;; Hooks

(defun centered-cursor--mode-unload-function ()
  (centered-cursor--log "--mode-unload-function")
  (global-centered-cursor-mode 0))

(defun centered-cursor--post-command-hook ()
  (centered-cursor-position-cursor))

;; TODO das reicht nicht, z.B. Overlays wie in origami verändern Anzahl Zeilen
(defun centered-cursor--after-change-function (_beg _end _old-length)
  (centered-cursor--reset-total-screen-lines-l))

(defun centered-cursor--window-configuration-change-hook ()
  ;; (centered-cursor--log "--window-configuration-change-hook: %s" centered-cursor--text-lines)
  (centered-cursor-calculate-position))

(defun centered-cursor--window-scroll-function (_window _display-start)
  (centered-cursor--reset-total-screen-lines-l); TODO passiert nach jedem recenter,
                                        ; also jedes mal → zu viel
  ;; (centered-cursor--log "window-start after scroll-function: %s" (line-number-at-pos display-start))
  ;; (redisplay t)
  )


;;; Mode definition

;;;###autoload
(define-minor-mode centered-cursor-mode
  "Makes the cursor stay vertically in a defined position (usually centered).

Except at the top of course, and at the bottom optionally. Scrolling behaves kind of like a
pager, having always a context around the cursor.
"
  :init-value nil
  :lighter centered-cursor-lighter
  :keymap centered-cursor-keymap
  ; ignore  mode in minibuffer or invisible buffers
  (unless (or (minibufferp)
              (string-match-p (rx string-start " *" (+? anything) "*" string-end)
                              (buffer-name)))
    (cond
     (centered-cursor-mode
      (centered-cursor--log "Centered-Cursor mode enabled")
      (centered-cursor--first-start (called-interactively-p 'interactive))
      (centered-cursor--add-hooks)
      (centered-cursor--set-local-mwheel-scroll-functions))
     (t
      (centered-cursor--log "Centered-Cursor mode disabled")
      (centered-cursor--remove-hooks)
      (centered-cursor--reset-local-mwheel-scroll-functions)))))
;long testlines for visual line mode ###################################################
;####################################### ######################################### ##################################################################################################################################################################################################################################
;;;###autoload
(define-globalized-minor-mode global-centered-cursor-mode centered-cursor-mode
  centered-cursor-mode
;  :global t
  :require 'centered-cursor-mode)

(provide 'centered-cursor-mode)

;;; Help:
;; (info "(elisp)Defining Minor Modes")
;; (info "(elisp)Screen Lines")
;; (info "(elisp)Hooks")
;; (info "(elisp)Customization")
;; (find-function 'mwheel-scroll)

;; Local Variables:
;; coding: utf-8
;; End:

;;; centered-cursor-mode.el ends here
