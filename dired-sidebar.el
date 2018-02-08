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
        ;; dired-sidebar-display-alist '(1 2 3 4)

        ;; dired-sidebar-face '(:background
        ;;                      "#202025"
        ;;                      :forground
        ;;                      "#bbc2cf"
        ;;                      :height
        ;;                      88
        ;;                      )

        ;; dired-sidebar-face '(:family "Arial" :height 140)
        ))

;; (use-package all-the-icons-dired
;;   ;; M-x all-the-icons-install-fonts
;;   :ensure t
;;   :commands (all-the-icons-dired-mode))
