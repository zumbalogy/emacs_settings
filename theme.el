(require 'doom-themes)

(fringe-mode 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-face-foreground 'vertical-border "#000")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-linum-mode)

(add-hook 'text-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'fundamental-mode-hook 'linum-mode)

(add-hook 'cider-repl-mode-hook #'linum-mode)

(setq linum-format "%3d ")

;; TODO: look into line numbers not showing up on newlines at bottom of file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default line-spacing 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (add-hook 'minibuffer-setup-hook
           (lambda ()
             (make-local-variable 'face-remapping-alist)
             (add-to-list 'face-remapping-alist
                          '(default (:foreground "#bbc2cf"))
                          ;; '(default (:background "red"))
                          )))

(set-face-foreground 'mode-line "#bbc2cf")
(set-face-background 'mode-line "#1c1e24")
(set-face-background 'mode-line-inactive "#1c1e24")

(set-face-attribute 'region nil :background "#42444a")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Global settings (defaults)
(setq doom-themes-enable-bold t)   ; if nil, bold is universally disabled
(setq doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
(load-theme 'doom-one t)
;; (load-theme 'atom-one-dark t)

;; Enable flashing mode-line on errors
;; (doom-themes-visual-bell-config)

;; Corrects (and improves) org-mode's native fontification.
;; (doom-themes-org-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-default-font "DejaVu Sans Mono")
