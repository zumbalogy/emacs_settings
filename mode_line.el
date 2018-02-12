(require 'cl)

;; TODO: made the modeline change color or
;; something when in remote/tramp mode

;; ;; https://elpa.gnu.org/packages/delight.html
;; (require 'delight)
;; (delight 'undo-tree-mode nil "abbrev")
;; (delight 'emacs-lisp-mode "elisp" "abbrev")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mode-line-details
  '("%e"
    ;; mode-line-front-space
    ;; mode-line-mule-info
    ;; mode-line-client
    ;; mode-line-modified
    ;; mode-line-remote
    ;; mode-line-frame-identification
    ;; mode-line-buffer-identification
    ;; " "
    ;; mode-line-position
    ;; "%c:%l"
    ;; (vc-mode vc-mode)
    ;; mode-line-modes
    ;; "%m" ;; major mode
    ;; mode-line-misc-info
    ;; mode-line-end-spaces
    ))

(setq *tabbar-ignore-regex* '("Treemacs-Framebuffer"
                              "Desktop Treemacs Helper"
                              "nrepl-server"
                              "cider-temp clojure-mode"))

(setq *tabbar-ignore-buffers* '("*Treemacs*"
                                "*Messages*"
                                "*Shell Command Output*"
                                "*scratch*"
                                "*server*"
                                " *server*"
                                "*Completions*"
                                " *code-conversion-work*"
                                " *Echo Area 0*"
                                " *Echo Area 1*"
                                " *Minibuf-0*"
                                " *Minibuf-1*"
                                " *cl-connection*"
                                " *slime-fontify*"
                                "*inferior-lisp*"
                                "*slime-events*"
                                ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mode-buffers ()
  (remove-if
   (lambda (buffer)
     (and (not (eq (current-buffer) buffer)) ; Always include the current buffer.
          (or (loop for regex in *tabbar-ignore-regex*
                    thereis (string-match-p regex (buffer-name buffer)))
              (loop for name in *tabbar-ignore-buffers*
                    thereis (string-equal name (buffer-name buffer))))))
   (buffer-list)))

(defun mode-line-split (left right)
  ;; this should maybe see how many lines the current buffer is
  ;; and how long the linums will be
  (let* ((available-width (- (window-width) (length left) -3))) ; linenums push this 3
    (format (format "%%s%%%ds" available-width) left right)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun propertize-name (name)
  (let* ((n (concat " " name " "))
         (selected (string= name (buffer-name (current-buffer)))))
    (if selected
        ;; (propertize n 'face '(:foreground "#4466bb" :box '(:line-width 5 :color "gray80")))
        (propertize n 'face '(:foreground "#4466bb" ))
      (propertize n 'face '(:foreground "#666666" )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sorted-tabs ()
  (sort (mode-buffers)
        (lambda (a b)
          (string< (buffer-name a)
                   (buffer-name b)))))

(defun mode-line-tabs ()
  (let* ((sorted-names (mapcar 'buffer-name (sorted-tabs)))
         (color-names (mapcar 'propertize-name sorted-names))
         (output (mapconcat 'identity color-names " ")))
    output))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-tab-forward (&optional arg)
  (interactive)
  (let* ((tabs (sorted-tabs))
         (pos (cl-position (current-buffer) tabs))
         (buffer (or (nth (+ pos 1) tabs)
                     (first tabs))))
    (switch-to-buffer buffer)))

(defun my-tab-backward (&optional arg)
  (interactive)
  (let* ((tabs (sorted-tabs))
         (pos (cl-position (current-buffer) tabs))
         (buffer (if (eq 0 pos)
                     (first (last tabs))
                   (nth (- pos 1) tabs))))
    (switch-to-buffer buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default mode-line-format
              '((:eval
                (mode-line-split
                 (format-mode-line 'mode-line-details)
                 (mode-line-tabs)))))

(global-set-key (kbd "C-S-<iso-lefttab>") 'my-tab-backward)
(global-set-key (kbd "C-<tab>") 'my-tab-forward)
