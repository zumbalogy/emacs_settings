(setq load-path (cons "~/emacs" load-path))


;; ln -s emacs/.emacs .emacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: have a file that i run only the first time i run emacs
;; (or have touched config files)

(require 'package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("f93b82e0ad0ea189fd981581fa66891aa78bd581a8a17931b4c644da3b18d7a6" "3273aa1448e0bfe6bf3d500f95e49f645743616a3116e88dfbdf982159f48c44" "64333355ad27d35db1ae47a9843a6b75f24e6192d74c97ae479286f9e445270e" "d56c707f683d5904415886a08f09c6b4724a3601477a7dbec1a15bc722935727" "" default)))
 '(package-selected-packages
   (quote
    (simpleclip dired-sidebar all-the-icons-dired treemacs-projectile treemacs cider auto-package-update cljdoc paredit parinfer haskell-emacs haml-mode color-theme-modern load-theme-buffer-local coffee-mode delight desktop+ fiplr undo-tree use-package symon smooth-scrolling smooth-scroll smex rainbow-delimiters multiple-cursors minibuffer-line el-get doom-themes color-theme-buffer-local clojure-mode-extra-font-locking atom-one-dark-theme))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(toggle-scroll-bar -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(set-face-attribute 'default nil :height 130)

(setq column-number-mode t)

(setq-default cursor-type 'bar)

(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; (show-paren-mode 1) 'package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((gc-cons-threshold 10000000))
  (package-initialize)
  ;; (byte-recompile-directory (expand-file-name "~/emacs") 0)
  (load "~/emacs/theme.el")
  (load "~/emacs/editing.el")
  (load "~/emacs/fiplr.el")
  ;; (load "~/emacs/tabs.el")
  (load "~/emacs/mode_line.el")
  (load "~/emacs/my_treemacs.el")
  ;; (load "~/emacs/dired-sidebar.el")
  ;; (load "~/emacs/my_neotree.el")
  ;; (load "~/emacs/mode_line_collapse.el")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'desktop)

(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  ;; (if (eq (desktop-owner) (emacs-pid)) ;; wont work with emacsclient
  ;;     (desktop-save desktop-dirname)))
  (desktop-save desktop-dirname))

(desktop-save-mode 1)
(setq desktop-restore-eager 12)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-hook 'auto-save-hook 'my-desktop-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t
   undo-tree-auto-save-history t
   undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(defvar undo-tree-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap redo] 'undo-tree-redo)
    (define-key map [remap undo] 'undo-tree-undo)
    (define-key map [remap undo-only] 'undo-tree-undo)
    (define-key map (kbd "M-_") 'undo-tree-redo)
    (define-key map (kbd "\C-x u") 'undo-tree-visualize)
    (define-key map (kbd "\C-x u") 'undo-tree-visualize)
    (define-key map (kbd "C-x r u") 'undo-tree-save-state-to-register)
    (define-key map (kbd "C-x r U") 'undo-tree-restore-state-from-register)
    (define-key map (kbd "C-/") nil)
    map))

(global-undo-tree-mode)

(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo-tree-undo)

(global-unset-key (kbd "C-y"))
(global-set-key (kbd "C-y") 'undo-tree-redo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(setq tooltip-use-echo-area t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun save-and-kill-this-buffer ()
  (interactive)
  (unless (or (string-equal "*" (substring (buffer-name) 0 1))
              (string-equal " *" (substring (buffer-name) 0 2)))
    (save-buffer))
  (unless (string-equal "*Treemacs*" (buffer-name))
    (kill-this-buffer)))

;; https://emacs.stackexchange.com/questions/3330/how-to-reopen-just-killed-buffer-like-c-s-t-in-firefox-browser
(defvar killed-file-list nil
  "List of recently killed files.")

;; TODO: have the kill-file-list capped at 24 files or so
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

;; scroll one line at a time (less "jumpy" than defaults)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: look into window-divider-mode
;; https://emacs.stackexchange.com/questions/29873/whats-this-line-between-the-modeline-and-the-echo-area

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'smex) ; Not needed if you use package.el
;; (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
;;                                         ; when Smex is auto-initialized on its first run.


;; To change ido-max-prospects. TODO: make this custom and submit pull request to
;; https://github.com/nonsequitur/smex
(defun smex-completing-read (choices initial-input)
  (let ((ido-completion-map ido-completion-map)
        (ido-setup-hook (cons 'smex-prepare-ido-bindings ido-setup-hook))
        (ido-enable-prefix nil)
        (ido-enable-flex-matching smex-flex-matching)
        (ido-max-prospects 5)
        (minibuffer-completion-table choices))
    (ido-completing-read (smex-prompt-with-prefix-arg) choices nil nil
                         initial-input 'extended-command-history (car choices))))

(setq smex-prompt-string "")

(if (eq window-system 'x)
    (shell-command "xmodmap -e 'clear Lock' -e 'keycode 66 = F13'"))

;; (global-set-key [f13] 'execute-extended-command)
(global-set-key [f13] 'smex)
(global-set-key (kbd "C-S-P") 'smex)
;; TODO: maybe have f13 (caps lock) cancel smex if smex is already up

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://www.masteringemacs.org/article/improving-performance-emacs-display-engine
(setq redisplay-dont-pause t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun display-startup-echo-area-message ()
  (message (current-time-string)))

(setq inhibit-startup-message nil)
(setq inhibit-startup-screen t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-command-error-function (data context caller)
  "Ignore the buffer-read-only signal; pass the rest to the default handler."
  (when (and (not (eq (car data) 'text-read-only))
             (not (eq (car data) 'text-read-only)))
    (command-error-default-function data context caller)))

(setq command-error-function #'my-command-error-function)

;; TODO: filter out messages like: beginning of buffer, end of buffer, quit, buffer is read only,
;;   command attempted to open minibuffer inside minibuffer
;; maybe loading messages as well. maybe save ones.
;; http://user.it.uu.se/~embe8573/conf/emacs-init/error.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  (load (expand-file-name "~/quicklisp/slime-helper.el"))
;;   Replace "sbcl" with the path to your implementation
;;  (setq inferior-lisp-program "sbcl")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cljdoc)
(require 'cider)
(add-hook 'clojure-mode-hook 'cider-mode)
(add-hook 'cider-interaction-mode-hook 'cider-turn-on-eldoc-mode)
