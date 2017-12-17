;; https://elpa.gnu.org/packages/delight.html
(require 'delight)
(delight 'undo-tree-mode nil "abbrev")
(delight 'emacs-lisp-mode "Elisp" "abbrev")


(defvar my-mode-line
  '("%e"
    ;; mode-line-front-space
    ;; mode-line-mule-info
    ;; mode-line-client
    ;; mode-line-modified
    ;; mode-line-remote
    ;; mode-line-frame-identification
    mode-line-buffer-identification
    "    "
    mode-line-position
    ;; (vc-mode vc-mode)
    mode-line-modes
    ;; "%m" ;; major mode
    mode-line-misc-info
    mode-line-end-spaces
    ))

(setq-default mode-line-format my-mode-line)

;; TODO: made the modeline change color or something when in remote mode
