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

(defun copy-line (&optional arg)
  "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (let* ((deactivate-mark nil)
           (pos-col (current-column))
           (top-col (save-excursion (goto-char (region-beginning)) (current-column)))
           (bot-col (save-excursion (goto-char (region-end)) (current-column)))
           (pos-line (line-number-at-pos))
           (top-line (line-number-at-pos (region-beginning)))
           (bot-line (line-number-at-pos (region-end)))
           (adjust (if (< 0 arg) 1 -1)))
      (copy-line)
      (goto-line top-line)
      (dotimes (_ (+ 1 (- bot-line top-line)))
        (kill-whole-line))
      (forward-line adjust)
      (yank)
      (if (= pos-line bot-line)
          (progn
            (goto-line (+ adjust top-line))
            (move-to-column top-col t)
            (push-mark (point))
            (activate-mark)
            (goto-line (+ adjust bot-line))
            (move-to-column bot-col t))
        (progn
          (goto-line (+ adjust bot-line))
          (move-to-column bot-col t)
          (push-mark (point))
          (activate-mark)
          (goto-line (+ adjust top-line))
          (move-to-column top-col t)))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(global-set-key [C-S-return] 'open-line)

(delete-selection-mode 1)

(setq save-interprogram-paste-before-kill t)
(setq x-select-enable-clipboard nil)

(defun cua-paste (&optional _)
  (interactive "p")
  (simpleclip-paste))

(defun cua-copy-region (&optional _)
  (interactive "p")
  (simpleclip-copy (region-beginning) (region-end)))

(defun cua-cut-region (&optional _)
  (interactive "p")
  (simpleclip-cut (region-beginning) (region-end)))

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

(defun select-line (&optional arg)
  (interactive "p")
  (beginning-of-line)
  (let ((oldval (or (cdr-safe transient-mark-mode) transient-mark-mode))
        (beg (point-marker)))
    (when mark-active
      (forward-line 1))
    (end-of-line)
    (unless mark-active
      (push-mark beg nil t))
    (setq transient-mark-mode (cons 'only oldval))))

(global-set-key (kbd "C-S-L") 'select-line)

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
;; https://github.com/clojure-emacs/cider/blob/master/doc/code_completion.md

(require 'cljdoc)
(require 'cider)

(setq nrepl-hide-special-buffers t
      cider-eldoc-display-context-dependent-info t)

(add-hook 'clojure-mode-hook 'cider-mode)
(add-hook 'cider-interaction-mode-hook 'cider-turn-on-eldoc-mode)

(add-hook 'cider-mode-hook #'eldoc-mode)

(add-hook 'cider-repl-mode-hook 'company-mode)
(add-hook 'cider-mode-hook 'company-mode)

(add-hook 'cider-repl-mode-hook 'cider-company-enable-fuzzy-completion)
(add-hook 'cider-mode-hook 'cider-company-enable-fuzzy-completion)

(setq company-idle-delay 0) ; default 0.5
;; (setq company-idle-delay nil) ; never start completions automatically

;; (setq ac-cider-show-ns nil)
;; (global-set-key (kbd "C-TAB") #'company-complete)

;; (global-set-key (kbd "TAB") #'company-indent-or-complete-common)

(define-key clojure-mode-map (kbd "C->") 'clojure-thread-first-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://martintrojer.github.io/clojure/2014/10/02/clojure-and-emacs-without-cider

(require 'company-etags)
(add-to-list 'company-etags-modes 'clojure-mode)

(defun get-clj-completions (prefix)
  (let* ((proc (inferior-lisp-proc))
         (comint-filt (process-filter proc))
         (kept ""))
    (set-process-filter proc (lambda (proc string) (setq kept (concat kept string))))
    (process-send-string proc (format "(complete.core/completions \"%s\")\n"
                                      (substring-no-properties prefix)))
    (while (accept-process-output proc 0.1))
    (setq completions (read kept))
    (set-process-filter proc comint-filt)
    completions))

(defun company-infclj (command &optional arg &rest ignored)
  (interactive (list 'interactive))

  (cl-case command
    (interactive (company-begin-backend 'company-infclj))
    (prefix (and (eq major-mode 'inferior-lisp-mode)
                 (company-grab-symbol)))
    (candidates (get-clj-completions arg))))

(add-to-list 'company-backends 'company-infclj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'clojure-mode-extra-font-locking)

;; Maybe i should make control-w close error buffers and such (any with a * maybe),
;; and their frame if its not the only frame. or, have C-S-W kill buffer and frame
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq cider-repl-result-prefix ";; => ")
(setq cider-interactive-eval-result-prefix ";; -> ")

(setq cider-repl-wrap-history t)
(setq cider-repl-history-size 10000)
(setq cider-repl-history-file (expand-file-name "~/.emacs.d/cider-repl.history"))
