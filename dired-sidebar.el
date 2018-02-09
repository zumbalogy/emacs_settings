(require 'projectile)

;; TODO: store all those undo-tree stuff and  other crap somewhere  else

(defun git-on-branch ()
  (let ((default-directory (projectile-project-root))
        (branch (shell-command-to-string "git branch | grep '*'")))
    (if (< 11 (length branch))
        (concat " " (substring branch 7 -4))
      "")))

(defvar sidebar-face
  '(:background "#202025" :foreground "#bbc2cf" :height 124))

(use-package dired-sidebar
  ;; :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-subtree-line-prefix " ."
        dired-sidebar-theme 'ascii
        dired-sidebar-use-term-integration t
        dired-sidebar-use-custom-font t
        dired-sidebar-width 24
        dired-sidebar-use-evil-integration nil
        dired-sidebar-mode-line-format (git-on-branch)
        dired-sidebar-face sidebar-face

        ;; dired-sidebar-display-alist '(1 2 3 4)

        ))

 ;; '(default ((t (:background "#202025" :foreground "#bbc2cf" :height 124))))

(add-hook 'dired-sidebar-mode-hook
          (lambda ()
            (linum-mode -1)
            ))
