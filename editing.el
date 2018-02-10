;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'multiple-cursors)
;; (require 'mouse)

;; (xterm-mouse-mode t)

;; (defun cancel-cursor-click (event)
;;   (interactive "e")
;;   (mouse-drag-region event)
;;   (mc/remove-fake-cursors))

;; (defun mark-whole-word (&optional arg allow-extend)
;;   "Like `mark-word', but selects whole words and skips over whitespace.
;; If you use a negative prefix arg then select words backward.
;; Otherwise select them forward.
;; If there is whitespace between the initial cursor position and the
;; first word (in the selection direction), it is skipped (not selected).
;; If the command is repeated or the mark is active, select the next NUM
;; words, where NUM is the numeric prefix argument.  (Negative NUM
;; selects backward.)"
;;   (interactive "P\np")
;;   (let ((num (prefix-numeric-value arg)))
;;     (unless (eq last-command this-command)
;;       (if (natnump num)
;;           (skip-syntax-forward "\\s-")
;;         (skip-syntax-backward "\\s-")))
;;     (unless (or (eq last-command this-command)
;;                 (if (natnump num)
;;                     (looking-at "\\b")
;;                   (looking-back "\\b")))
;;       (if (natnump num)
;;           (left-word)
;;         (right-word)))
;;     (mark-word arg allow-extend)))

;; (defun my-select-word ()
;;   (interactive "p")
;;   (message (number-to-string
;;             (cl-count-if 'mc/fake-cursor-p
;; 	                   (overlays-in (point-min) (point-max)))))
;;   (if (= 1 (mc/num-cursors))
;;       (mark-whole-word)
;;     (mc/mark-next-like-this-word)))

;; (global-unset-key (kbd "C-<down-mouse-1>"))
;; (global-set-key (kbd "C-<mouse-1>") 'mc/add-cursor-on-click)

;; (global-unset-key (kbd "<down-mouse-1>"))
;; (global-set-key (kbd "<down-mouse-1>") 'cancel-cursor-click)

;; (global-unset-key (kbd "C-d"))
;; (global-set-key (kbd "C-d") 'my-select-word)
;; (global-set-key (kbd "C-S-d") 'mc/mark-all-like-this)

;; (define-key mc/keymap (kbd "<return>") nil)

;; ;; press C-' to hide all lines without a cursor, press C-' again to unhide

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: This does not write comment on empty line, and also does not
;; keep things highlighted
(defun my-comment-line (n)
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region
       (save-excursion
         (goto-char (region-beginning))
         (line-beginning-position))
       (save-excursion
         (goto-char (region-end))
         (line-end-position)))
    (when (and (eq last-command 'comment-line-backward)
               (natnump n))
      (setq n (- n)))
    (let ((range
           (list (line-beginning-position)
                 ;; (goto-char (line-end-position n)))))
                 (line-end-position n))))
      (comment-or-uncomment-region
       (apply #'min range)
       (apply #'max range)))
    ;; (forward-line 1)
    ;; (back-to-indentation)
    (unless (natnump n)
      (setq this-command 'comment-line-backward))))

(global-unset-key (kbd "C-;"))
(global-set-key (kbd "C-;") 'my-comment-line)

(global-unset-key (kbd "C-/"))
(global-set-key (kbd "C-/") 'my-comment-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun duplicate-region (&optional n)
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1))
                          (newline))))))
        (dotimes (i (abs (or n 1)))
          (insert text))))
    (if use-region nil
      (let ((pos (- (point) (line-beginning-position)))) ; Save column
        (forward-line 1)
        (forward-char pos)))))

(global-unset-key (kbd "C-S-d"))
(global-set-key (kbd "C-S-d") 'duplicate-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun quick-cut-line ()
  "Cut the whole line that point is on.  Consecutive calls to this command
    append each line to the kill-ring."
  (interactive)
  (let ((beg (line-beginning-position 1))
	(end (line-beginning-position 2)))
    (if (eq last-command 'quick-cut-line)
	(kill-append (buffer-substring beg end) (< end beg))
      (kill-new (buffer-substring beg end)))
    (delete-region beg end))
  (beginning-of-line 1)
  (setq this-command 'quick-cut-line))

(global-unset-key (kbd "C-S-k"))
(global-set-key (kbd "C-S-k") 'quick-cut-line)

;; make this line or region

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
      (forward-line -1)))))

(defun move-text-down (arg)
  (interactive "*p")
  (save-column
   (move-text-internal arg)))

(defun move-text-up (arg)
  (interactive "*p")
  (save-column
   (move-text-internal (- arg))))

(global-set-key (kbd "C-S-<up>") 'move-text-up)
(global-set-key (kbd "C-S-<down>") 'move-text-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour
(require 'simpleclip)

(defun nonbreaking-return (&optional n)
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (end-of-line)
    (newline)
    (indent-according-to-mode)))

(with-eval-after-load 'cua-base
  (define-key cua-global-keymap [C-return] nil)
  (global-set-key [(C-return)] 'nonbreaking-return))

(delete-selection-mode 1)

(setq save-interprogram-paste-before-kill t)
(setq x-select-enable-clipboard nil)

(defun cua-paste (&optional _)
  (interactive "p")
  (simpleclip-paste))

(defun cua-copy-region (&optional _)
  (interactive "p")
  (simpleclip-copy (region-beginning) (region-end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun my-smart-tab (&optional n)
;;   (interactive "*p")
;;   (parinfer-smart-tab:dwim-right)
;;   (parinfer--invoke-parinfer))

;; (use-package parinfer
;;   :ensure t
;;   :bind
;;   (:map parinfer-mode-map
;;         ("C-," . parinfer-toggle-mode)
;;         ("<tab>" . my-smart-tab))
;;   :config
;;   (parinfer-strategy-add 'default 'newline-and-indent)
;;   :init
;;   (progn
;;     (setq parinfer-extensions
;;           '(defaults       ; should be included.
;;              pretty-parens  ; different paren styles for different modes.
;;              paredit        ; Introduce some paredit commands.
;;              smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
;;              smart-yank))   ; Yank behavior depend on mode.
;;     (add-hook 'clojure-mode-hook #'parinfer-mode)
;;     (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
;;     (add-hook 'common-lisp-mode-hook #'parinfer-mode)
;;     (add-hook 'scheme-mode-hook #'parinfer-mode)
;;     (add-hook 'lisp-mode-hook #'parinfer-mode)))

;; (setq parinfer-auto-switch-indent-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'clojure-mode)

(define-clojure-indent
  (for-all 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'clojure-mode-hook          #'enable-paredit-mode)

(defun paredit-delete-indentation (&optional arg)
  "Handle joining lines that end in a comment."
  (interactive "*P")
  (let (comt)
    (save-excursion
      (move-beginning-of-line (if arg 1 0))
      (when (skip-syntax-forward "^<" (point-at-eol))
        (setq comt (delete-and-extract-region (point) (point-at-eol)))))
    (delete-indentation arg)
    (when comt
      (save-excursion
        (move-end-of-line 1)
        (insert " ")
        (insert comt)))))

(require 'paredit)

(define-key paredit-mode-map (kbd "M-^") 'paredit-delete-indentation)

(define-key paredit-mode-map (kbd "C-<right>") nil)
(define-key paredit-mode-map (kbd "C-<left>") nil)

(define-key paredit-mode-map (kbd "M-<DEL>") 'paredit-kill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-forward (&optional arg)
  "Move ARG times to start of a set of the same syntax characters."
  (interactive "p")
  (setq arg (or arg 1))
  (while (and (> arg 0)
              (not (eobp))
              (skip-syntax-forward (string (char-syntax (char-after)))))
    (setq arg (1- arg)))
  (while (and (< arg 0)
              (not (bobp))
              (skip-syntax-backward
               (string (char-syntax (char-before)))))
    (setq arg (1+ arg))))

(defun my-backward (&optional arg)
  (interactive "p")
  (my-forward (- (or arg 1))))

(defun my-select-forward (&optional arg)
  (interactive "p")
  (let ((oldval (or (cdr-safe transient-mark-mode) transient-mark-mode))
        (backwards (and mark-active (> (mark) (point))))
        (beg (and mark-active (mark-marker))))
    (unless beg
      (setq beg (point-marker)))
    (my-forward)
    (unless mark-active
      (push-mark beg nil t))
    (setq transient-mark-mode (cons 'only oldval))))

(defun my-select-backward (&optional arg)
  (interactive "p")
  (let ((oldval (or (cdr-safe transient-mark-mode) transient-mark-mode))
        (backwards (and mark-active (> (mark) (point))))
        (beg (and mark-active (mark-marker))))
    (unless beg
      (setq beg (point-marker)))
    (my-backward)
    (unless mark-active
      (push-mark beg nil t))
    (setq transient-mark-mode (cons 'only oldval))))

(defun my-control-delete (&optional arg)
  (interactive "p")
  (my-select-forward)
  (kill-region (region-beginning) (region-end)))

(defun my-control-backspace (&optional arg)
  (interactive "p")
  (my-select-backward)
  (kill-region (region-beginning) (region-end)))

(global-unset-key (kbd "C-<right>"))
(global-unset-key (kbd "C-<left>"))

(global-set-key (kbd "C-<right>") 'my-forward)
(global-set-key (kbd "C-<left>") 'my-backward)

(global-set-key (kbd "C-S-<right>") 'my-select-forward)
(global-set-key (kbd "C-S-<left>") 'my-select-backward)

(global-unset-key [C-delete])
(global-unset-key [C-backspace])

(global-set-key [C-delete] 'my-control-delete)
(global-set-key [C-backspace] 'my-control-backspace)

(defun my-backspace (&optional arg)
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-backward-chars (or arg 1))))

(defun my-delete (&optional arg)
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-forward-chars (or arg 1))))

(global-unset-key [delete])
(global-unset-key [backspace])

(global-set-key [delete] 'my-delete)
(global-set-key [backspace] 'my-backspace)

(define-key paredit-mode-map [delete] nil)
(define-key paredit-mode-map [delete] 'my-delete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mark-whole-word (&optional arg allow-extend)
  (interactive "P\np")
  (let ((num (prefix-numeric-value arg)))
    (unless (eq last-command this-command)
      (if (natnump num)
          (skip-syntax-forward "\\s-")
        (skip-syntax-backward "\\s-")))
    (unless (or (eq last-command this-command)
                (if (natnump num)
                    (looking-at "\\b")
                  (looking-back "\\b")))
      (if (natnump num)
          (left-word)
        (right-word)))
    (mark-word arg allow-extend)))

(global-unset-key (kbd "C-d"))
(define-key paredit-mode-map (kbd "C-d") nil)

(global-set-key (kbd "C-d") 'mark-whole-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-key paredit-mode-map (kbd "C-9") nil)
(define-key paredit-mode-map (kbd "C-0") nil)

(define-key paredit-mode-map (kbd "C-9") 'paredit-backward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-0") 'paredit-forward-slurp-sexp)

(define-key paredit-mode-map (kbd "C-)") nil)
(define-key paredit-mode-map (kbd "C-(") nil)

(define-key paredit-mode-map (kbd "C-)") 'paredit-backward-barf-sexp)
(define-key paredit-mode-map (kbd "C-(") 'paredit-forward-barf-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
;; (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
