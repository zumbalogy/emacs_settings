(require 'treemacs)
(require 'treemacs-persist)

(use-package treemacs
  :ensure t
  :defer t
  :config (progn
            (treemacs-follow-mode t)
            (treemacs-filewatch-mode t)
            (setq treemacs-follow-after-init          t
                  treemacs-width                      28
                  treemacs-indentation                1
                  treemacs-git-integration            t
                  treemacs-collapse-dirs              0
                  treemacs-silent-refresh             t
                  treemacs-change-root-without-asking nil
                  treemacs-sorting                    'alphabetic-desc
                  treemacs-show-hidden-files          t
                  treemacs-no-images                  t
                  treemacs-no-png-images              t
                  treemacs-never-persist              nil
                  treemacs-is-never-other-window      t
                  treemacs-goto-tag-strategy          'refetch-index
                  treemacs-icon-open-png (propertize "- " 'face 'treemacs-directory-face)
                  treemacs-icon-closed-png (propertize "+ " 'face 'treemacs-directory-face)
                  )))

(defun treemacs-header (current-root)
  (format " %s " (file-name-nondirectory current-root)))

(setq treemacs-header-function #'treemacs-header)

;; TODO: have it follow version control of a buffer
;; TODO: would be nice to color the background properly
;; (set-face-attribute 'hl-line nil :foreground "black" :background "yellow")

(defun git-on-branch ()
  (let ((default-directory (treemacs--current-root))
        (branch (shell-command-to-string "git branch | grep '*'")))
    (if (< 11 (length branch))
        (concat " " (substring branch 7 -4))
      "")))

(defvar tree-mode-line-format
  (list '(:eval (git-on-branch))))

(define-derived-mode treemacs-mode special-mode "Treemacs"
  (setq-local mode-line-format tree-mode-line-format)
  (add-hook 'post-command-hook 'force-mode-line-update nil t))

;; todo: make git colors update quicker, and maybe make the whole thing black and white
;; or less colorful.

;; TODO: ignore certain file types (like *.elc)

(defun my-treemacs-click-action (event)
  "Move focus to the clicked line.
Must be bound to a mouse click, or EVENT will not be supplied."
  (interactive "e")
  (when (eq 'mouse-1 (elt event 0))
    (unless (eq major-mode 'treemacs-mode)
      ;; no when-let - the window must exist or this function would not be called
      (select-window (treemacs--is-visible?)))
    (goto-char (posn-point (cadr event)))
    (when (region-active-p)
      (keyboard-quit))
    ;; 7th element is the clicked image
    (treemacs-do-for-button-state
     :on-dir-node-closed  (treemacs--expand-dir-node btn)
     :on-dir-node-open    (treemacs--collapse-dir-node btn)
     :on-file-node-closed (treemacs-visit-node-no-split btn)
     :on-file-node-open   (treemacs-visit-node-no-split btn)
     :no-error            t)
    (treemacs--evade-image)))
;; TODO: get this to move focus to new buffer

;; also, look into dired-sidebar, as its faster
;; and treemacs is now super slow.
;; just have to figure out nice sorting
;; for that one.



(require 'color-theme-buffer-local)
(add-to-list 'custom-theme-load-path "~/emacs/themes")

(add-hook 'treemacs-mode-hook
          (lambda ()
            (linum-mode -1)
            (load-theme-buffer-local 'my-treemacs (get-buffer " *Treemacs-Framebuffer-1*"))
            (define-key treemacs-mode-map [mouse-1] 'my-treemacs-click-action)
            ))
