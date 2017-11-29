(setq load-path (cons "~/emacs" load-path))

;; ln -s emacs/.emacs .emacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: have a file that i run only the first time i run emacs
;; (or have touched config files)

;; (require 'package)
;; (require 'desktop)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((gc-cons-threshold 10000000))
  (package-initialize)
  (byte-recompile-directory (expand-file-name "~/emacs") 0)
  (load "~/emacs/theme.el")
  (load "~/emacs/editing.el")
  (load "~/emacs/tabs.el")
  (load "~/emacs/tree.el")
  ;; (load "~/emacs/mode_line.el")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-desktop-save ()
   (interactive)
   ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))

(desktop-save-mode 1)
(setq desktop-restore-eager 7)
(add-hook 'auto-save-hook 'my-desktop-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun save-and-kill-this-buffer ()
  (interactive)
  (save-buffer)
  (kill-this-buffer))

;; https://emacs.stackexchange.com/questions/3330/how-to-reopen-just-killed-buffer-like-c-s-t-in-firefox-browser
(defvar killed-file-list nil
  "List of recently killed files.")

(defun add-file-to-killed-file-list ()
  "If buffer is associated with a file name, add that file to the
`killed-file-list' when killing the buffer."
  (when buffer-file-name
    (push buffer-file-name killed-file-list)))

(add-hook 'kill-buffer-hook #'add-file-to-killed-file-list)

(defun reopen-killed-file ()
  "Reopen the most recently killed file, if one exists."
  (interactive)
  (when killed-file-list
    (find-file (pop killed-file-list))))


(global-set-key [(control w)] 'save-and-kill-this-buffer)
(global-set-key [(control shift t)] 'reopen-killed-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun my-quit ()
;;   (save-buffers-kill-terminal))

;; (global-unset-key [(control q)])
;; (global-set-key [(control q)] 'my-quit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-undo-tree-mode)

(setq undo-tree-auto-save-history t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: maybe make it so it ignores git ignored things by default

(setq fiplr-ignored-globs '((directories (".git" ".svn" ".hg" ".bzr" "target"))
                            (files (".#*" "*~" "*.so" "*.o" "*.jpg" "*.png" "*.gif" "*.pdf" "*.gz" "*.zip"))))


(global-unset-key [(control t)])
(global-set-key [(control t)] 'fiplr-find-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-command-error-function (data context caller)
  "Ignore the buffer-read-only signal; pass the rest to the default handler."
   (when (not (eq (car data) 'text-read-only))
     (command-error-default-function data context caller)))


(setq command-error-function #'my-command-error-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load "foo"
  (toggle-scroll-bar -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (set-face-attribute 'default nil :height 130))

(setq column-number-mode t)

(setq-default cursor-type 'bar)

(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; (show-paren-mode 1) 'package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; scroll one line at a time (less "jumpy" than defaults)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: look into window-divider-mode
;; https://emacs.stackexchange.com/questions/29873/whats-this-line-between-the-modeline-and-the-echo-area

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'smex) ; Not needed if you use package.el
;; (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                                        ; when Smex is auto-initialized on its first run.

(setq smex-prompt-string "")

(if (eq window-system 'x)
    (shell-command "xmodmap -e 'clear Lock' -e 'keycode 66 = F13'"))

;; (global-set-key [f13] 'execute-extended-command)
(global-set-key [f13] 'smex)
;; TODO: maybe have f13 (caps lock) cancel smex if smex is already up


;; (global-unset-key (kbd "M-x"))
;; (global-set-key (kbd "M-x") 'smex)
;; (global-unset-key (kbd "M-X"))
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.

;; this is ugly/lots of visual noise, but ok.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://www.masteringemacs.org/article/improving-performance-emacs-display-engine
(setq redisplay-dont-pause t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (fiplr undo-tree use-package treemacs-projectile tabbar symon smooth-scrolling smooth-scroll smex rainbow-delimiters neotree multiple-cursors minibuffer-line el-get doom-themes color-theme-buffer-local clojure-mode-extra-font-locking cider atom-one-dark-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
