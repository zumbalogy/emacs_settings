;; NOTE: scroll to cycle though tab groups

(require 'tabbar)

(tabbar-mode 1)

(setq tbbr-md "all")
;; (setq *tabbar-ignore-buffers* '("tab-ignore-example" ".bbdb" "diary"))

;; Tabbar settings
(set-face-attribute 'tabbar-default nil
                    :background "gray20"
                    :foreground "gray20"
                    :box '(:line-width 5 :color "gray20" :style nil)
                    :height 1.1)
(set-face-attribute 'tabbar-unselected nil
                    :background "gray30"
                    :foreground "white"
                    :box '(:line-width 5 :color "gray30" :style nil))
(set-face-attribute 'tabbar-selected nil
                    :background "gray75"
                    :foreground "black"
                    :box '(:line-width 5 :color "gray75" :style nil))
(set-face-attribute 'tabbar-highlight nil
                    :background "white"
                    :foreground "black"
                    :underline nil
                    :box '(:line-width 5 :color "white" :style nil))
(set-face-attribute 'tabbar-button nil
                    :foreground "white"
                    :background "gray20"
                    :box '(:line-width 5 :color "gray20" :style nil))
(set-face-attribute 'tabbar-separator nil
                    :background "gray30"
                    :height 0.6)

;; adding spaces
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format "[%s]  " (tabbar-tab-tabset tab))
                  (format "%s  " (tabbar-tab-value tab)))))
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
   (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
               ((eq major-mode 'dired-mode) "emacs")
               (t "user"))))

(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

(global-set-key [(control shift tab)] 'tabbar-backward-tab)
(global-set-key [(control tab)] 'tabbar-forward-tab)

;; TODO: tabbar would maybe be better as a modeline (or message to echo area)
;; and could just have control-tab rotate though a group of tabs
;; or at least dont need file name in mode line

;; TODO: maybe group tabs by what buffer they were opened in
;; thus it would be more like atom/chrome tabs

;; TODO: if only one tab, should hide bar maybe.

;; This gets rid of tabbar buttons. maybe scroll should scroll if there are more
;; buttons than fit on the line, and control+scroll or something could cycle groups
(defsubst tabbar-line-buttons (tabset)
  "Return a list of propertized strings for tab bar buttons.
TABSET is the tab set used to choose the appropriate buttons."
  (list tabbar-separator-value))
