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
    mode-line-misc-info
    mode-line-end-spaces))


(setq-default mode-line-format my-mode-line)


;; TODO: get rid of minor modes i dont like in the modeline (like undo tree)
;; https://emacs.stackexchange.com/questions/3925/hide-list-of-minor-modes-in-mode-line

;; TODO: made the modeline change color or something when in remote mode
