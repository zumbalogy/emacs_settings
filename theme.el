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
(set-face-foreground 'vertical-border "#000")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-linum-mode 1)

(setq linum-format "%3d ")

;; TODO: look into line numbers not showing up on newlines at bottom of file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default line-spacing 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'color-theme-buffer-local)
;; (add-hook 'foo-mode-hook (lambda nil (color-theme-buffer-local 'color-theme-robin-hood (current-buffer)) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-face-background 'mode-line-inactive "#1c1e24") ; to match active one
;; TODO: right now anything darker or lighter kinda looks like the inactive
;; buffer is the active buffer. so, i should make some way of representing that and all
;; maybe have current line (or linum) highlighted or something

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(dolist (buf '(" *Echo Area 0*" " *Echo Area 1*"))
  (with-current-buffer (get-buffer buf)
    (make-local-variable 'face-remapping-alist)
    (add-to-list 'face-remapping-alist '(default (:foreground "#bbc2cf")))))

(add-hook 'minibuffer-setup-hook
          (lambda ()
            (make-local-variable 'face-remapping-alist)
            (add-to-list 'face-remapping-alist
                         '(default (:forground "#bbc2cf")))))

;; TODO: if set background, it works far all this except when buffers are changed
;; and there is just blank space there, and running the echo area code
;; does not fill it in. but this might not be a problem if i move
;; tabs (or a default time message) down there.
;; forground works to set because its only when its blank is it a problem

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
