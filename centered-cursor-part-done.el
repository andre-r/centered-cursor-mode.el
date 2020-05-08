;; Done parts (except docstrings)

;;; Customization

(defgroup centered-cursor nil ""
  :group 'scrolling
  :group 'convenience
  :link '(emacs-library-link :tag "Source Lisp File" "centered-cursor-mode.el")
  :link '(url-link "https://github.com/andre-r/centered-cursor-mode.el"))

(defcustom centered-cursor-lighter
  :group 'centered-cursor
  :tag "Mode line symbol"
  :type '(choice (string :tag "Custom string" :format "%{%t%}: %v" :size 10)
                 (const :tag "None" :value "")))

(make-obsolete-variable 'ccm-step-size nil "0.7")
(make-obsolete-variable 'ccm-step-delay nil "0.7")
(make-obsolete-variable 'ccm-vpos-init 'centered-cursor-initial-position "0.7")

(define-widget 'centered-cursor--command-widget 'function
  "A command."
  :tag "Command"
  :completions (apply-partially #'completion-table-with-predicate
                                obarray #'commandp 'strict)
  :match-alternatives '(commandp)
  :validate (lambda (widget)
	          (unless (commandp (widget-value widget))
		        (widget-put widget :error (format "Invalid command: %S"
						                          (widget-value widget)))
		        widget)))

(define-obsolete-variable-alias 'ccm-ignored-commands 'centered-cursor-ignored-commands "0.7")
(defcustom centered-cursor-ignored-commands '(mouse-drag-region
                                              mouse-set-region
                                              mouse-set-point
                                              widget-button-click
                                              scroll-bar-toolkit-scroll
                                              handle-select-window
                                              tabbar-select-tab-callback)
  "todo"
  :group 'centered-cursor
  :tag "Ignored commands"
  :type '(repeat centered-cursor--command-widget))


(defcustom centered-cursor-initial-position 'centered
  "todo"
  :group 'centered-cursor
  :tag "Vertical cursor position" ;; TODO description
  :type '(choice (const :tag "Centered" centered)
                 (const :tag "Golden ratio" golden-ratio)
                 ;; const e.g. ... '(centered-cursor-initial-position 'golden-ratio) ...
                 (const :tag "Golden ratio from bottom" golden-ratio-from-bottom)
                 ;; cons e.g. ... '(centered-cursor-initial-position '(ratio . 0.4)) ...
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
                                              (widget-put widget :error (format "Ratio must be between 0.0 and 1.0: %S" value))
                                              widget)))))
                 (cons :tag "Lines from top"
                       :format "%t: %v\n"
                       (const :format "" lines-from-top)
                       (integer :format "%v"
                                :value 10
                                :size 5
                                :validate (lambda (widget)
                                            (let ((value (widget-value widget)))
                                              (when (< value 0)
                                                (widget-put widget :error (format "Value must be positive: %S" values))
                                                widget)))))
                 (cons :tag "Lines from bottom"
                       :format "%t: %v\n"
                       (const :format "" lines-from-bottom)
                       (integer :format "%v"
                                :value 10
                                :size 5
                                :validate (lambda (widget)
                                            (let ((value (widget-value widget)))
                                              (when (< value 0)
                                                (widget-put widget :error (format "Value must be positive: %S" values))
                                                widget)))))
                 (cons :tag "Custom function"
                       :format "%t: %v"
                       (const :format "" custom-function)
                       (function :format "%v"))))
(make-variable-buffer-local 'centered-cursor-initial-position)

(define-obsolete-variable-alias 'ccm-recenter-at-end-of-file 'centered-cursor-recenter-at-end-of-buffer "0.7")
(defcustom centered-cursor-recenter-at-end-of-buffer t
  "For performance reasons default t ... Warning: slow ..."
  :group 'centered-cursor
  :tag "Recenter at end of buffer"
  :type '(choice (const :tag "Don't recenter at the end of the file" nil)
                 (const :tag "Recenter at the end of the file" t)))
(make-variable-buffer-local 'centered-cursor-recenter-at-end-of-buffer)


;;; keymap and keys

(define-obsolete-variable-alias 'ccm-keymap 'centered-cursor-keymap "0.7")
(defvar centered-cursor-keymap (make-sparse-keymap))

;;;###autoload
(defun centered-cursor-default-bindings ()
  (interactive)
  (define-key centered-cursor-keymap (kbd "C-M--") 'ccm-set-pos-up)
  (define-key centered-cursor-keymap (kbd "C-M-+") 'ccm-set-pos-down)
  (define-key centered-cursor-keymap (kbd "C-M-=") 'ccm-set-pos-down)
  (define-key centered-cursor-keymap (kbd "C-M-0")
    'centered-cursor-calculate-position))


;;; TODO
(defvar centered-cursor-bindings
  '((define-key centered-cursor-keymap (kbd "C-M--") 'ccm-set-pos-up)
    (define-key centered-cursor-keymap (kbd "C-M-+") 'ccm-set-pos-down)
    (define-key centered-cursor-keymap (kbd "C-M-=") 'ccm-set-pos-down)
    (define-key centered-cursor-keymap (kbd "C-M-0")
      'centered-cursor-calculate-position))
  "List of default binding forms evaluated by `centered-cursor-bindings' by using

  (centered-cursor-bindings)

in the init file.

List can be adjusted with own bindings. Before calling
`centered-cursor-bindings' of course.")

;;;###autoload
(defun centered-cursor-bindings ()
  "Evaluate the forms in variable `centered-cursor-bindings'."
  (interactive)
  (eval (cons 'progn centered-cursor-bindings)))


;;; Overriding functions and compatibility

(define-key centered-cursor-keymap [remap scroll-down-command] 'ccm-scroll-down)
(define-key centered-cursor-keymap [remap scroll-up-command] 'ccm-scroll-up)

;;;###autoload
(defun centered-cursor-View-scroll-page-backward (&optional lines)
  "TODO"
  (interactive "P")
  (let ((lines (or lines (view-page-size-default view-page-size))))
    (centered-cursor-scroll-down lines)))
(define-key centered-cursor-keymap [remap View-scroll-page-backward] 'centered-cursor-View-scroll-page-backward)

;;;###autoload
(defun centered-cursor-View-scroll-page-forward (&optional lines)
  (interactive "P")
  (let ((lines (or lines (view-page-size-default view-page-size))))
    (centered-cursor-scroll-up lines)))
(define-key centered-cursor-keymap [remap View-scroll-page-forward] 'centered-cursor-View-scroll-page-forward)


;; TODO autoload?
(defun centered-cursor--Info-scroll-down--around (orig-fun &rest _args)
  (condition-case ex
      (funcall orig-fun)
    (error (progn
             (centered-cursor-scroll-down)))))
(advice-add 'Info-scroll-down :around 'centered-cursor--Info-scroll-down--around)

(defun centered-cursor--Info-scroll-up--around (orig-fun &rest _args)
  (condition-case ex
      (funcall orig-fun)
    (error (progn
             (centered-cursor-scroll-up)))))
(advice-add 'Info-scroll-up :around 'centered-cursor--Info-scroll-up--around)

;; Kompatibilität: mwheel
;; TODO: andere?
(defvar-local centered-cursor--original-mwheel-scroll-up-function 'scroll-up)

(defvar-local centered-cursor--original-mwheel-scroll-down-function 'scroll-down)

(defun centered-cursor--set-local-mwheel-scroll-functions ()
  (setq centered-cursor--original-mwheel-scroll-up-function
        mwheel-scroll-up-function)
  (setq centered-cursor--original-mwheel-scroll-down-function
        mwheel-scroll-down-function)
  (setq mwheel-scroll-up-function 'next-line)
  (setq mwheel-scroll-down-function 'previous-line))

(defun centered-cursor--reset-local-mwheel-scroll-functions ()
  (setq mwheel-scroll-up-function
        centered-cursor--original-mwheel-scroll-up-function)
  (setq mwheel-scroll-down-function
        centered-cursor--original-mwheel-scroll-down-function))


;;; Logging

(defun centered-cursor--log-top (string &rest objects)
  "Internal log function for logging variables. See `centered-cursor--log-top-values'."
  (when centered-cursor--log-p
    (let ((debug-buffer
           (let ((name "*centered-cursor-log*"))
             (or (get-buffer name)
                 (generate-new-buffer name)))))
      (with-current-buffer debug-buffer
        (goto-char (point-min))
        (delete-region (point-min)
                       (1+ (progn
			                 (search-forward (char-to-string ?\^L) nil t)
			                 (point))))
        (insert (apply #'format
                       (concat string (char-to-string ?\^L) "\n")
                       objects))))))

(defun centered-cursor--log (string &rest objects)
  "Internal log function for logging messages."
  (when centered-cursor--log-p
    (let ((debug-buffer
           (let ((name "*centered-cursor-log*"))
             (or (get-buffer name)
                 (generate-new-buffer name))))
          (buffer (buffer-name)))
      (let ((message (apply #'format (concat string "\n") objects))
            hlinepos hlineline)
        (with-current-buffer debug-buffer
          (goto-char (point-min))
          (setq hlinepos
                (or (search-forward (concat (char-to-string ?\^L) "\n") nil t)
                    (point-min)))
          (setq hlineline (line-number-at-pos hlinepos))
          (while (> (count-lines (point-min) (point-max))
                   (+ hlineline 20))
            (goto-char hlinepos)
            (delete-region (line-beginning-position 1) (line-beginning-position 2)))
          (goto-char (point-max))
          (insert (concat (format-time-string "%F %T") " [" buffer "] " message)))))))

(defun centered-cursor--log-top-values ()
  (centered-cursor--log-top
   "Values before recentering:
==========================

last-command-event:  %s
---> mouse-event-p:  %s
this-command:        %s
last-command:        %s
current-visual-line/max: %3s/%3s
visual-text-lines: %s
centered-cursor-position: %s
delta: %s
"
   last-command-event
   (mouse-event-p last-command-event)
   this-command
   last-command

   (centered-cursor--visual-line-number-at-pos (point))
   (centered-cursor--total-screen-lines)
   (window-text-height)

   centered-cursor-position
   (centered-cursor--visual-line-diff ccm--old-point (point))))

;;; position manipulating

(defun centered-cursor--default-scroll-amount ()
  "Default scroll amount for `centered-cursor--scroll' is a near full screen,
which is `window-text-height' minus `next-screen-context-lines'.
See also `scroll-down-command'."
  (- (window-text-height)
     next-screen-context-lines))

(defun centered-cursor--scroll (arg prefix-func)
  "Internal function for scrolling up or down. Used by
`centered-cursor-scroll-up' and `centered-cursor-scroll-down'
for page up or down and mouse wheel. Uses `line-move'."
  (let ((amt (apply prefix-func
                    (list (or arg
                              (centered-cursor--default-scroll-amount))))))
    (line-move amt)))

;;;###autoload
(defun centered-cursor-scroll-up (&optional arg)
  (interactive "P")
  (centered-cursor--scroll arg #'identity))
(define-obsolete-function-alias 'ccm-scroll-up 'centered-cursor-scroll-up "0.7")

;;;###autoload
(defun centered-cursor-scroll-down (&optional arg)
  (interactive "P")
  (centered-cursor--scroll arg #'-))
(define-obsolete-function-alias 'ccm-scroll-down 'centered-cursor-scroll-down "0.7")

(defun centered-cursor--jump-recenter ()
  ;; (ccm--animated-recenter recenter-arg delta lines-to-window-bottom eob-in-sight-p lines-to-eob+1)
  (ccm--recenter)
  (pulse-momentary-highlight-one-line (point)))

(defun centered-cursor-calculate-position ()
  "Calculates and sets the vertical cursor position (the line) acording to the
customization."
;; called in centered-cursor--first-start, centered-cursor--refresh and as
;; interactive command to reset position
  (interactive)
  (when (equal (current-buffer)
               (window-buffer (selected-window)))
    (setq centered-cursor-position
          (condition-case ex ; exception handling
              (let ((init centered-cursor-initial-position)
                    (height (window-text-height)))
                (if (consp init)
                    ;; e.g. (ratio . 0.4)
                    (let ((sym (car init))
                          (value (cdr init)))
                      (cond
                       ((eq sym 'ratio)
                        (setq value (max 0.0 value))
                        (setq value (min 1.0 value))
                        (round (* height value)))
                       ((eq sym 'lines-from-top)
                        (setq value (max 0 value))
                        (setq value (min height value))
                        value)
                       ((eq sym 'lines-from-bottom)
                        (setq value (max 0 value))
                        (setq value (min height value))
                        (- height value))
                       ((eq sym 'custom-function)
                        (setq value (funcall value))
                        (when (not (numberp value))
                          (signal 'wrong-type-argument value))
                        (if (integerp value)
                            (progn
                              (setq value (max 0 value))
                              (setq value (min height value)))
                          (setq value (max 0.0 value))
                          (setq value (min 1.0 value)))
                        value)))
                  ;; else init not cons but const
                  (cond
                   ((eq init 'centered)
                    (round height 2))
                   ((eq init 'golden-ratio)
                    (round (* 21 height) 34))
                   ((eq init 'golden-ratio-from-bottom)
                    (- height (round (* 21 height) 34))))))
            (error (progn
                     ;; return default value "centered"" if customized value is faulty
                     (message "Error in `centered-cursor-initial-position'. Defaulting to `centered'")
                     (round height 2)))))))


;; Hooks

(defvar centered-cursor--hook-alist
  '((post-command-hook . centered-cursor--post-command-hook)
    (after-change-functions . centered-cursor--after-change-function)
    (window-configuration-change-hook . centered-cursor--window-configuration-change-hook)
    (window-scroll-functions . centered-cursor--window-scroll-function)
    (text-scale-mode-hook . centered-cursor--window-configuration-change-hook)))

(defun centered-cursor--add-hooks ()
  (mapc (lambda (entry)
          (add-hook (car entry) (cdr entry) t t))
        centered-cursor--hook-alist))

(defun centered-cursor--remove-hooks ()
  (mapc (lambda (entry)
          (remove-hook (car entry) (cdr entry) t))
        centered-cursor--hook-alist))


(provide 'ccm-dev)
