(require 'treemacs)
(require 'treemacs-persist)

(use-package treemacs
  :ensure t
  :defer t
  :config (progn
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
                  treemacs-never-persist              nil
                  treemacs-is-never-other-window      t
                  treemacs-goto-tag-strategy          'refetch-index
                  )
            (treemacs-follow-mode t)
            (treemacs-filewatch-mode t)))

(defun treemacs-header (current-root)
  (format " %s " (file-name-nondirectory current-root)))

(setq treemacs-header-function #'treemacs-header)

(setq treemacs-icon-fallback (propertize "  " 'face 'treemacs-term-node-face))
(setq treemacs-icon-closed (propertize "+ " 'face 'treemacs-term-node-face))
(setq treemacs-icon-open (propertize "- " 'face 'treemacs-term-node-face))
(setq treemacs-icons-hash (make-hash-table))

(add-hook 'treemacs-mode-hook '(lambda () (linum-mode -1)))

(with-eval-after-load "treemacs"
  (treemacs-restore)
  (define-key treemacs-mode-map [mouse-1] 'treemacs-visit-node-default-action))

;; TODO: have it follow version control of a buffer
;; TODO: hide "Treemacs" text from the modeline (maybe show git branch)
;; TODO: would be nice to color the tree background

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
