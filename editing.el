;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(delete-selection-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

;; (defun atom-C-d ()
;;   (interactive)
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
;; (global-set-key (kbd "C-d") 'atom-C-d)
;; (global-set-key (kbd "C-S-d") 'mc/mark-all-like-this)

;; (define-key mc/keymap (kbd "<return>") nil)

;; ;; press C-' to hide all lines without a cursor, press C-' again to unhide

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar undo-tree-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap undo] 'undo-tree-undo)
    (define-key map [remap undo-only] 'undo-tree-undo)
    (define-key map [remap redo] 'undo-tree-redo)
    (define-key map (kbd "M-_") 'undo-tree-redo)
    (define-key map (kbd "\C-x u") 'undo-tree-visualize)
    (define-key map (kbd "\C-x u") 'undo-tree-visualize)
    (define-key map (kbd "C-x r u") 'undo-tree-save-state-to-register)
    (define-key map (kbd "C-x r U") 'undo-tree-restore-state-from-register)
    map))

(global-undo-tree-mode)
(setq undo-tree-auto-save-history t)

(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo-tree-undo)

(global-unset-key (kbd "C-y"))
(global-set-key (kbd "C-y") 'undo-tree-redo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
    With argument N, make N copies.
    With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(global-unset-key (kbd "C-S-d"))
(global-set-key (kbd "C-S-d") 'duplicate-line-or-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun quick-cut-line ()
  "Cut the whole line that point is on.  Consecutive calls to this command append each line to the kill-ring."
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

; make this line or region


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

(global-set-key (kbd "C-S-<up>") 'move-text-up)
(global-set-key (kbd "C-S-<down>") 'move-text-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
