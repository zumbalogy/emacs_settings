(use-package treemacs
  :ensure t
  :defer nil
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
                  treemacs-icons-hash                 (make-hash)
                  )
            (treemacs-follow-mode t)
            (treemacs-filewatch-mode t)))

(defun treemacs-header-with-brackets (current-root)
  (format "%s" (file-name-nondirectory current-root)))

(setq treemacs-header-function #'treemacs-header-with-brackets)

(setq treemacs-icon-fallback (propertize "  " 'face 'treemacs-term-node-face))
(setq treemacs-icon-closed (propertize "+ " 'face 'treemacs-term-node-face))
(setq treemacs-icon-open (propertize "- " 'face 'treemacs-term-node-face))

;; (with-eval-after-load "treemacs"
;;   (maphash (lambda (key value)
;;              (puthash key treemacs-icon-fallback treemacs-icons-hash))
;;            treemacs-icons-hash)
;;   (define-key treemacs-mode-map [mouse-1] 'treemacs-visit-node-default-action))

;; TODO: have it follow version control of a buffer
;; TODO: hide "Treemacs" text from the modeline (maybe show git branch)
;; TODO: would be nice to color the tree background

;; (set-face-attribute 'hl-line nil :foreground "black" :background "yellow")
