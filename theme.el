;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fringe-mode 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(load-theme 'atom-one-dark t)

(require 'doom-themes)

;; Global settings (defaults)
(setq doom-themes-enable-bold t)   ; if nil, bold is universally disabled
(setq doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
(load-theme 'doom-one t)

;; Enable flashing mode-line on errors
;; (doom-themes-visual-bell-config)

;; Corrects (and improves) org-mode's native fontification.
;; (doom-themes-org-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (set-face-background 'vertical-border "red")
(set-face-foreground 'vertical-border "gray")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-linum-mode 1)

(setq linum-format "%-3d ")

;; TODO: look into line numbers not showing up on newlines at bottom of file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
