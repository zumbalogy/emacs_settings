(require 'tabbar)

(tabbar-mode 1)

(setq-default header-line-format nil)

(setq tbbr-md "all")

(defun my-tabbar-buffer-groups () (list "user"))

(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

(setq *tabbar-ignore-regex* '("Treemacs-Framebuffer"))

(setq *tabbar-ignore-buffers* '("*Treemacs*"
                                "*Messages*"
                                "*Shell Command Output*"
                                "*scratch*"
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

;; Sort by name
(defun tabbar-add-tab (tabset object &optional append_ignored)
  (let ((tabs (tabbar-tabs tabset)))
    (if (tabbar-get-tab object tabset)
        tabs
      (let ((tab (tabbar-make-tab object tabset)))
        (tabbar-set-template tabset nil)
        (set tabset (sort (cons tab tabs)
                          (lambda (a b) (string< (buffer-name (car a)) (buffer-name (car b))))))))))

(require 'cl) ;; TODO: rewrite this to not need cl
(setq tabbar-buffer-list-function
      (lambda ()
        (remove-if
         (lambda (buffer)
           (and (not (eq (current-buffer) buffer)) ; Always include the current buffer.
                (or (loop for regex in *tabbar-ignore-regex*
                          thereis (string-match-p regex (buffer-name buffer)))
                    (loop for name in *tabbar-ignore-buffers*
                          thereis (string-equal name (buffer-name buffer))))))
         (buffer-list))))

;; (global-set-key (kbd "C-S-<iso-lefttab>") 'tabbar-backward-tab)
;; (global-set-key (kbd "C-<tab>") 'tabbar-forward-tab)
