
(use-package treemacs
  :ensure t
  :defer t
  :config (progn
            (setq treemacs-follow-after-init          t
                  treemacs-width                      30
                  treemacs-indentation                1
                  treemacs-git-integration            t
                  treemacs-collapse-dirs              0
                  treemacs-silent-refresh             t
                  treemacs-change-root-without-asking nil
                  treemacs-sorting                    'alphabetic-desc
                  treemacs-show-hidden-files          t
                  treemacs-never-persist              nil
                  treemacs-is-never-other-window      nil
                  treemacs-goto-tag-strategy          'refetch-index)
            (treemacs-follow-mode t)
            (treemacs-filewatch-mode t))
  :bind (:map global-map
              ([f8]         . treemacs-toggle)
              ("M-0"        . treemacs-select-window)
              ("C-c 1"      . treemacs-delete-other-windows)
              ("C-c 2"      . treemacs-find-file)))

(use-package treemacs-projectile
  :defer t
  :ensure t
  :config (setq treemacs-header-function #'treemacs-projectile-create-header))

;; (with-eval-after-load "treemacs"
;;   (defvar treemacs-custom-html-icon nil)
;;   (treemacs-define-custom-icon treemacs-custom-html-icon "clj"))

(defun treemacs-header-with-brackets (current-root)
  (format "%s" (file-name-nondirectory current-root)))

(setq treemacs-header-function #'treemacs-header-with-brackets)

;; TODO: have it follow version control of a buffer
;; TODO: no icons
