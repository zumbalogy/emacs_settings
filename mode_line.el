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
    " "
    ;; mode-line-position
    "%c:%l:%i"
    ;; (vc-mode vc-mode)
    ;; mode-line-modes
    ;; "%m" ;; major mode
    mode-line-misc-info
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
  (let* ((available-width (- (window-width) (length left) 1)))
    ;; (format (format "%%s %%%ds" available-width) left right)))
    (format (format "%%s%%%ds" available-width) left right)))

(defun mode-line-tabs ()
  (let* ((buffers (mode-buffers))
         (names (mapconcat 'buffer-name buffers "  ")))
    names))

(setq-default mode-line-format
              '((:eval
                (mode-line-split
                 (format-mode-line 'mode-line-details)
                 (mode-line-tabs)))))
