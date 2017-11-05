(setq load-path (cons "~/emacs" load-path))

;; ln -s emacs/.emacs .emacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(when (>= emacs-major-version 24)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "~/emacs/theme.el")
(load "~/emacs/editing.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun save-all ()
  (interactive)
  (when (buffer-modified-p)
    (save-some-buffers t)))

(add-hook 'focus-out-hook 'save-all)
;; (add-hook 'kill-buffer-query-functions 'save-all) ; this seems to prevent killing
;; unless there are changes, or something like that.

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; TODO: make it so it does not kill whitespace on current line
;; (unless closing buffer/file, i guess)
;; also make it kill the buffer for real (and save) when i say kill the buffer

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-command-error-function (data context caller)
  "Ignore the buffer-read-only signal; pass the rest to the default handler."
   (when (not (eq (car data) 'text-read-only))
     (command-error-default-function data context caller)))


(setq command-error-function #'my-command-error-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(toggle-scroll-bar -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq column-number-mode t)

(setq-default cursor-type 'bar)

(set-face-attribute 'default nil :height 130)

(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; (show-paren-mode 1) 'package)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'desktop)
(desktop-save-mode 1)
(defun my-desktop-save ()
   (interactive)
   ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))

(add-hook 'auto-save-hook 'my-desktop-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; scroll one line at a time (less "jumpy" than defaults)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'minibuffer-line)

;; ;; (setq minibuffer-line-format '((:eval
;; ;; 				(let ((time-string (format-time-string "%l:%M %b %d %a"))
;; ;; 				      (let ((end-string (concat time-string)))
;; ;; 					(let ((padding (make-string (- (frame-text-cols) (string-width end-string) ?))))
;; ;;                                    (concat padding end-string))))))))


;; (defun strip-text-properties (txt)
;;   (set-text-properties 0 (length txt) nil txt)
;;   txt)

;; (setq minibuffer-line-format
;;       '((:eval
;; 	 (let ((time-string (format-time-string "%s"))
;; 	       (mode-string (format-mode-line mode-line-format)))
;; 	   (let ((end-string (concat time-string mode-string)))
;; 	     (let ((padding (make-string (- (frame-text-cols) (string-width end-string)) ? )))
;; 	       (concat padding end-string)))))))


;; (minibuffer-line-mode)

;; TODO: put mode line stuff in the echo area maybe, and hide mode line.
;; (make sure to still indicate active buffer somehow)
;; maybe scoll bars and just the line number would save space at the bottom mode line

;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; variable for the timer object
;; (defvar idle-timer-cookbook-timer 1xxzx)

;; ;; callback function
;; (defun idle-timer-cookbook-callback ()
;;   (message "I have been called (%s)" (current-time-string)))

;; ;; start functions
;; (defun idle-timer-cookbook-run-once ()
;;   (interactive)
;;   (when (timerp idle-timer-cookbook-timer)
;;     (cancel-timer idle-timer-cookbook-timer))
;;   (setq idle-timer-cookbook-timer
;;           (run-with-idle-timer 1 nil #'idle-timer-cookbook-callback)))

;; (defun idle-timer-cookbook-start ()
;;   (interactive)
;;   (when (timerp idle-timer-cookbook-timer)
;;     (cancel-timer idle-timer-cookbook-timer))
;;   (setq idle-timer-cookbook-timer
;;           (run-with-timer 1 1 #'idle-timer-cookbook-callback)))

;; ;; stop function
;; (defun idle-timer-cookbook-stop ()
;;   (interactive)
;;   (when (timerp idle-timer-cookbook-timer)
;;     (cancel-timer idle-timer-cookbook-timer))
;;   (setq idle-timer-cookbook-timer nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'symon)

;; (defvar my-mode-line-format
;;   '("%e"
;;     mode-line-front-space
;;     mode-line-mule-info
;;     mode-line-client
;;     mode-line-modified
;;     mode-line-remote
;;     mode-line-frame-identification
;;     mode-line-buffer-identification
;;     mode-line-position
;;     (vc-mode vc-mode)
;;     mode-line-modes
;;     mode-line-misc-info
;;     mode-line-end-spaces))

;; (define-symon-monitor symon-mode-line
;;   :display (propertize (format-mode-line my-mode-line-format)))

;; (setq symon-delay 0)
;; (setq symon-history-size 10)

;; (setq symon-monitors '(symon-mode-line))

;; (setq-default mode-line-format nil)

;; (symon-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (with-current-buffer " *Echo Area*" (face-remap-add-relative 'default :background "#222"))
;; (with-current-buffer " *Echo Area 1*" (face-remap-add-relative 'default :background "#222"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: look into window-divider-mode
;; https://emacs.stackexchange.com/questions/29873/whats-this-line-between-the-modeline-and-the-echo-area

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (treemacs beacon tabbar smex symon smooth-scrolling smooth-scroll rainbow-delimiters neotree multiple-cursors minibuffer-line el-get doom-themes clojure-mode-extra-font-locking cider atom-one-dark-theme)))
 '(tabbar-separator (quote (0.5))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                                        ; when Smex is auto-initialized on its first run.

(global-unset-key (kbd "M-x"))
(global-set-key (kbd "M-x") 'smex)
(global-unset-key (kbd "M-X"))
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.

;; this is ugly/lots of visual noise, but ok.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://www.masteringemacs.org/article/improving-performance-emacs-display-engine
(setq redisplay-dont-pause t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'tabbar)
(tabbar-mode 1)
(setq tbbr-md "all")
(setq *tabbar-ignore-buffers* '("tab-ignore-example" ".bbdb" "diary"))

;; Tabbar settings
(set-face-attribute
 'tabbar-default nil
 :background "gray20"
 :foreground "gray20"
 :box '(:line-width 1 :color "gray20" :style nil)
 :height 1.1)
(set-face-attribute
 'tabbar-unselected nil
 :background "gray30"
 :foreground "white"
 :box '(:line-width 5 :color "gray30" :style nil))
(set-face-attribute
 'tabbar-selected nil
 :background "gray75"
 :foreground "black"
 :box '(:line-width 5 :color "gray75" :style nil))
(set-face-attribute
 'tabbar-highlight nil
 :background "white"
 :foreground "black"
 :underline nil
 :box '(:line-width 5 :color "white" :style nil))
(set-face-attribute
 'tabbar-button nil
 :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute
 'tabbar-separator nil
 :background "gray20"
 :height 0.6)
(set-face-attribute
 'tabbar-default nil
 :background "gray20"
 :foreground "gray20"
 :box '(:line-width 1 :color "gray20" :style nil))

;; Change padding of the tabs
;; we also need to set separator to avoid overlapping tabs by highlighted tabs

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package treemacs
  :ensure t
  :defer t
  :config (progn
            (setq treemacs-follow-after-init          t
                  treemacs-width                      30
                  treemacs-indentation                1
                  treemacs-git-integration            t
                  treemacs-collapse-dirs              0
                  treemacs-silent-refresh             nil
                  treemacs-change-root-without-asking nil
                  treemacs-sorting                    'alphabetic-desc
                  treemacs-show-hidden-files          t
                  treemacs-never-persist              nil
                  treemacs-is-never-other-window      nil
                  treemacs-goto-tag-strategy          'refetch-index)
            (treemacs-follow-mode t)
            (treemacs-filewatch-mode t))
  :bind (:map global-map
              ([f8]         . treemacs-toggle)
              ("M-0"        . treemacs-select-window)
              ("C-c 1"      . treemacs-delete-other-windows)
              ("C-c 2"      . treemacs-find-file)))

(use-package treemacs-projectile
  :defer t
  :ensure t
  :config (setq treemacs-header-function #'treemacs-projectile-create-header))

;; (with-eval-after-load "treemacs"
;;   (defvar treemacs-custom-html-icon nil)
;;   (treemacs-define-custom-icon treemacs-custom-html-icon "clj"))

(defun treemacs-header-with-brackets (current-root)
  (format "%s" (file-name-nondirectory current-root)))

(setq treemacs-header-function #'treemacs-header-with-brackets)

;; TODO: have it follow version control of a buffer
;; TODO: no icons


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
