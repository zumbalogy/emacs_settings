(require 'tabbar)

(tabbar-mode 1)

(setq tbbr-md "all")
;; (setq *tabbar-ignore-buffers* '("tab-ignore-example" ".bbdb" "diary"))

;; Tabbar settings
(set-face-attribute 'tabbar-default nil
                    :background "gray40"
                    :foreground "white"
                    :height 1.2
                    :box '(:line-width 5 :color "gray40" :style nil))
(set-face-attribute 'tabbar-selected nil
                    :background "gray80"
                    :foreground "black"
                    :bold nil
                    :box '(:line-width 5 :color "gray80" :style nil))
(set-face-attribute 'tabbar-unselected nil
                    :background "gray40"
                    :foreground "black")
(set-face-attribute 'tabbar-highlight nil
                    :background "gray80"
                    :foreground "black"
                    :underline nil
                    :box '(:line-width 5 :color "gray80" :style nil))
(set-face-attribute 'tabbar-modified nil
                    :bold nil
                    :foreground "black")
(set-face-attribute 'tabbar-selected-modified nil
                    :bold nil
                    :foreground "black")
(set-face-attribute 'tabbar-separator nil
                    :box '(:line-width 5 :color "#1c1e24" :style nil))

;; adding spaces
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format "[%s]  " (tabbar-tab-tabset tab))
                  (format "%s " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

 (defun my-tabbar-buffer-groups () ;; customize to show all normal files in one group
   "Returns the name of the tab group names the current buffer belongs to.
 There are two groups: Emacs buffers (those whose name starts with '*', plus
 dired buffers), and the rest.  This works at least with Emacs v24.2 using
 tabbar.el v1.7."
   (list (cond
          ;; ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
          ;; ((eq major-mode 'dired-mode) "emacs")
          (t "user"))))

(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

;; (defun my-filter (condp lst)
;;   (delq nil
;;         (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

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

;; (defun tabbar-add-tab (tabset object &optional append_ignored)
;;   "Add to TABSET a tab with value OBJECT if there isn't one there yet.
;;  If the tab is added, it is added at the beginning of the tab list,
;;  unless the optional argument APPEND is non-nil, in which case it is
;;  added at the end."
;;   (let ((tabs (tabbar-tabs tabset)))
;;     (if (tabbar-get-tab object tabset)
;;         tabs
;;       (let ((tab (tabbar-make-tab object tabset)))
;;         (tabbar-set-template tabset nil)
;;         (set tabset (sort (cons tab tabs)
;;                           (lambda (a b) (string< (buffer-name (car a)) (buffer-name (car b))))))))))

;; TODO: tabbar would maybe be better as a modeline (or message to echo area)
;; or at least dont need file name in mode line

;; TODO: maybe group tabs by what buffer they were opened in
;; thus it would be more like atom/chrome tabs

;; TODO: if only one tab, should hide bar maybe.

;; This gets rid of tabbar buttons. maybe scroll should scroll if there are more
;; buttons than fit on the line, and control+scroll or something could cycle groups
(defsubst tabbar-line-buttons (tabset)
  "Return a list of propertized strings for tab bar buttons.
TABSET is the tab set used to choose the appropriate buttons."
  (list (propertize "")))

(defun tabbar-line-format (tabset)
  "Return the `header-line-format' value to display TABSET."
  (let* ((sel (tabbar-selected-tab tabset))
         (tabs (tabbar-view tabset))
         (padcolor (tabbar-background-color))
         atsel elts)
    ;; Initialize buttons and separator values.
    (or tabbar-separator-value
        (tabbar-line-separator))
    (or tabbar-home-button-value
        (tabbar-line-button 'home))
    (or tabbar-scroll-left-button-value
        (tabbar-line-button 'scroll-left))
    (or tabbar-scroll-right-button-value
        (tabbar-line-button 'scroll-right))
    ;; Track the selected tab to ensure it is always visible.
    (when tabbar--track-selected
      (while (not (memq sel tabs))
        (tabbar-scroll tabset -1)
        (setq tabs (tabbar-view tabset)))
      (while (and tabs (not atsel))
        (setq elts  (cons (tabbar-line-tab (car tabs)) elts)
              atsel (eq (car tabs) sel)
              tabs  (cdr tabs)))
      (setq elts (nreverse elts))
      ;; At this point the selected tab is the last elt in ELTS.
      ;; Scroll TABSET and ELTS until the selected tab becomes
      ;; visible.
      (with-temp-buffer
        (let ((truncate-partial-width-windows nil)
              (inhibit-modification-hooks t)
              deactivate-mark ;; Prevent deactivation of the mark!
              start)
          (setq truncate-lines nil
                buffer-undo-list t)
          (apply 'insert (tabbar-line-buttons tabset))
          (setq start (point))
          (while (and (cdr elts) ;; Always show the selected tab!
                      (progn
                        (delete-region start (point-max))
                        (goto-char (point-max))
                        (apply 'insert elts)
                        (goto-char (point-min))
                        (> (vertical-motion 1) 0)))
            (tabbar-scroll tabset 1)
            (setq elts (cdr elts)))))
      (setq elts (nreverse elts))
      (setq tabbar--track-selected nil))
    ;; Format remaining tabs.
    (while tabs
      (setq elts (cons (tabbar-line-tab (car tabs)) elts)
            tabs (cdr tabs)))
    ;; Cache and return the new tab bar.
    (tabbar-set-template
     tabset
     (list (tabbar-line-buttons tabset)
           (nreverse elts)
           (propertize "%"
                       'face (list :background padcolor
                                   :foreground padcolor)
                       'pointer 'arrow)))
    ))
