;; TODO: made the modeline change color or something when in remote mode

;; ;; https://elpa.gnu.org/packages/delight.html
;; (require 'delight)
;; (delight 'undo-tree-mode nil "abbrev")
;; (delight 'emacs-lisp-mode "elisp" "abbrev")



(defvar mode-line-details
  '("%e"
    ;; mode-line-front-space
    ;; mode-line-mule-info
    ;; mode-line-client
    ;; mode-line-modified
    ;; mode-line-remote
    ;; mode-line-frame-identification
    ;; mode-line-buffer-identification
    ;; "    "
    ;; " "
    ;; mode-line-position
    ;; "%c:%l"
    ;; (vc-mode vc-mode)
    ;; mode-line-modes
    ;; "%m" ;; major mode
    ;; mode-line-misc-info
    ;; mode-line-end-spaces
    ))

(require 'cl) ;; TODO: rewrite this to not need cl
(defun mode-buffers ()
  (remove-if
   (lambda (buffer)
     (and (not (eq (current-buffer) buffer)) ; Always include the current buffer.
          (loop for name in *tabbar-ignore-buffers* ;remove buffer name in this list.
                thereis (string-equal (buffer-name buffer) name))))
   (buffer-list)))

(defun mode-line-split (left right)
  (let* ((available-width (- (window-width) (length left) -3))) ; linenums push this 3
    ;; (format (format "%%s %%%ds" available-width) left right)))
    (format (format "%%s%%%ds" available-width) left right)))

(defun propertize-name (name)
  (let* ((n (concat " " name " "))
         (selected (string= name (buffer-name (current-buffer)))))
    (if selected
        (propertize n 'face '(:foreground "#4466bb" :box '(:line-width 5 :color "gray80")))
        (propertize n 'face '(:foreground "#666666" :box '(:line-width 5 :color "gray80"))))))

(defun mode-line-tabs ()
  (let* ((buffers (mode-buffers))
         (names (mapcar 'buffer-name buffers))
         (sorted-names (sort names 'string<))
         (color-names (mapcar 'propertize-name sorted-names))
         (output (mapconcat 'identity color-names " ")))
    output))

(setq-default mode-line-format
              '((:eval
                (mode-line-split
                 (format-mode-line 'mode-line-details)
                 (mode-line-tabs)))))
