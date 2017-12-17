(require 'tabbar)

(tabbar-mode 1)

(setq-default header-line-format nil)

(setq tbbr-md "all")

(defun my-tabbar-buffer-groups () (list "user"))

(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

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
                (loop for name in *tabbar-ignore-buffers* ;remove buffer name in this list.
                      thereis (string-equal (buffer-name buffer) name))))
         (buffer-list))))

(global-set-key [(control shift tab)] 'tabbar-backward-tab)
(global-set-key [(control tab)] 'tabbar-forward-tab)