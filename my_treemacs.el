(require 'treemacs)
(require 'treemacs-persist)

(use-package treemacs
  :ensure t
  :defer t
  :config (progn
            (treemacs-follow-mode t)
            (treemacs-filewatch-mode t)
            (setq treemacs-follow-after-init          t
                  treemacs-file-event-delay           300 ;ms
                  treemacs-width                      28
                  treemacs-indentation                2
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
  (interactive "e")
  (when (eq 'mouse-1 (elt event 0))
    (unless (eq major-mode 'treemacs-mode)
      (select-window (treemacs--is-visible?)))
    (goto-char (posn-point (cadr event)))
    (when (region-active-p)
      (keyboard-quit))
    (treemacs-do-for-button-state
     :on-dir-node-closed  (treemacs--expand-dir-node btn)
     :on-dir-node-open    (treemacs--collapse-dir-node btn)
     :on-file-node-closed (treemacs-visit-node-no-split btn)
     :on-file-node-open   (treemacs-visit-node-no-split btn)
     :no-error            t)
    (treemacs--evade-image)
    (other-window 1)))
;; TODO: get this to move focus to new buffer

;; also, look into dired-sidebar, as its faster
;; just have to figure out nice sorting for it.

(require 'color-theme-buffer-local)
(add-to-list 'custom-theme-load-path "~/emacs/themes")

(add-hook 'treemacs-mode-hook
          (lambda ()
            (linum-mode -1)
            (load-theme-buffer-local 'my-treemacs (treemacs--get-framelocal-buffer))
            (define-key treemacs-mode-map [mouse-1] 'my-treemacs-click-action)
            (treemacs-git-mode 'simple)))

(defun my-treemacs-refresh ()
  (let ((inhibit-message t))
    (if (treemacs--is-visible?)
        (treemacs-refresh)
      (treemacs-projectile))
    (when (string= (buffer-name (treemacs-buffer-exists?)) (buffer-name))
      (other-window 1))))

(add-hook 'focus-in-hook #'my-treemacs-refresh)
(add-hook 'focus-out-hook #'my-treemacs-refresh)

(add-to-list 'after-make-frame-functions
             (lambda (_)
               (run-at-time "0 sec" nil #'my-treemacs-refresh)))
