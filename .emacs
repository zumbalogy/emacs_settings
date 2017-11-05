(setq load-path (cons "~/emacs" load-path))

; ln -s emacs/.emacs .emacs

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(load-theme 'atom-one-dark t)

(require 'doom-themes)

;; Global settings (defaults)
(setq doom-themes-enable-bold t)   ; if nil, bold is universally disabled
(setq doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
(load-theme 'doom-one t)

;; Enable flashing mode-line on errors
;; (doom-themes-visual-bell-config)

;; Enable custom neotree theme
;;(doom-themes-neotree-config)  ; all-the-icons fonts must be installed!
;;(setq doom-neotree-enable-file-icons nil)
;;(setq doom-neotree-enable-folder-icons nil)
;;(setq doom-neotree-enable-chevron-icons nil)
;;(setq doom-neotree-file-icons nil)

;; Corrects (and improves) org-mode's native fontification.
;; (doom-themes-org-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: comment and uncomment block/s-exp via control slash.
;; dont have mode line print if im doing a search though text
;; autosave before quit
;; also, copy, paste, undo, redo, move line up/down one, select all
;; equvalent of atom control shift, insert new line below current one and put cursor on it. dont break current line
;; control-d (or such) to select current word/thing and
;;     control-d while something is selected to also select next occcurence, w/ mutli cursor
;; multiple cursors
;; cider
;; autocomplete/tab-complete in editor?
;; better scrolling (smooth scrolling didnt work)
;; repl/terminal/etc in emacs
;; scroll up for history (in repl, in echo area maybe, etc)
;; send to terminal?
;; learn org mode
;; better orginize .emacs
;; emacs git integration (git porcelin?)
;;   atom shows what lines are uncommited in git, which is maybe useful. maybe i could toggle to show dif or something
;; better git repo support for emacs configs
;;   (.emacs should just link to ~/emacs/ and ~/emacs/.emacs and ~/emacs/ should be git repo, im guessing)
;; learn/get better find and find/replace (find in folder/repo/etc) (dired? helm?)
;; learn/get better file/project navigation (neotree?) (control-k in atom?)
;;    ?? https://github.com/bbatsov/projectile
;; fuzzy find stuff (helm?)
;; better toggle though buffers and frames. (but mostly frames)
;; tweak timeouts on echo area messages and all that
;; filter some messages (or tweak them) that go in echo area
;;    (https://www.emacswiki.org/emacs/EchoArea)
;; https://github.com/Fuco1/smartparens
;; paredit
;; https://github.com/Malabarba/aggressive-indent-mode
;; https://github.com/ogdenwebb/emacs-kaolin-themes/wiki/Kaolin-dark-theme
;; have paste try to indent corrently
;; control-enter should make new line without breaking current line
;; http://emacsredux.com/blog/2013/06/25/boost-performance-by-leveraging-byte-compilation/
;; fuzzy finding for command tab completion (type eval-bufer and tab should make it eval-buffer, or something like that)
;; if i start to type in an argument, up should take me though my history that matches that prefix, like i have in my terminal
;; https://github.com/Malabarba/beacon
;; https://github.com/wasamasa/eyebrowse
;; https://www.emacswiki.org/emacs/Icicles
;; https://github.com/syohex/emacs-git-gutter

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fringe-mode 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: look into window-divider-mode
;; https://emacs.stackexchange.com/questions/29873/whats-this-line-between-the-modeline-and-the-echo-area

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(delete-selection-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'multiple-cursors)
(require 'mouse)

(xterm-mouse-mode t)

(defun cancel-cursor-click (event)
  (interactive "e")
  (mouse-drag-region event)
  (mc/remove-fake-cursors))

(defun mark-whole-word (&optional arg allow-extend)
  "Like `mark-word', but selects whole words and skips over whitespace.
If you use a negative prefix arg then select words backward.
Otherwise select them forward.
If there is whitespace between the initial cursor position and the
first word (in the selection direction), it is skipped (not selected).
If the command is repeated or the mark is active, select the next NUM
words, where NUM is the numeric prefix argument.  (Negative NUM
selects backward.)"
  (interactive "P\np")
  (let ((num (prefix-numeric-value arg)))
    (unless (eq last-command this-command)
      (if (natnump num)
          (skip-syntax-forward "\\s-")
        (skip-syntax-backward "\\s-")))
    (unless (or (eq last-command this-command)
                (if (natnump num)
                    (looking-at "\\b")
                  (looking-back "\\b")))
      (if (natnump num)
          (left-word)
        (right-word)))
    (mark-word arg allow-extend)))

(defun atom-C-d ()
  (interactive)
  (message (number-to-string
            (cl-count-if 'mc/fake-cursor-p
	                   (overlays-in (point-min) (point-max)))))
  (if (= 1 (mc/num-cursors))
      (mark-whole-word)
    (mc/mark-next-like-this-word)))

(global-unset-key (kbd "C-<down-mouse-1>"))
(global-set-key (kbd "C-<mouse-1>") 'mc/add-cursor-on-click)

(global-unset-key (kbd "<down-mouse-1>"))
(global-set-key (kbd "<down-mouse-1>") 'cancel-cursor-click)

(global-unset-key (kbd "C-d"))
(global-set-key (kbd "C-d") 'atom-C-d)
(global-set-key (kbd "C-S-d") 'mc/mark-all-like-this)

(define-key mc/keymap (kbd "<return>") nil)

;; press C-' to hide all lines without a cursor, press C-' again to unhide

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun toggle-comment ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

(global-unset-key (kbd "C-;"))
(global-set-key (kbd "C-;") 'toggle-comment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
    With argument N, make N copies.
    With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(global-unset-key (kbd "C-S-d"))
(global-set-key (kbd "C-S-d") 'duplicate-line-or-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun quick-cut-line ()
  "Cut the whole line that point is on.  Consecutive calls to this command append each line to the kill-ring."
  (interactive)
  (let ((beg (line-beginning-position 1))
	(end (line-beginning-position 2)))
    (if (eq last-command 'quick-cut-line)
	(kill-append (buffer-substring beg end) (< end beg))
      (kill-new (buffer-substring beg end)))
    (delete-region beg end))
  (beginning-of-line 1)
  (setq this-command 'quick-cut-line))

(global-unset-key (kbd "C-S-k"))
(global-set-key (kbd "C-S-k") 'quick-cut-line)

; make this line or region


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

(global-set-key (kbd "C-S-<up>") 'move-text-up)
(global-set-key (kbd "C-S-<down>") 'move-text-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                  treemacs-width                      26
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
  :config (setq treemacs-header-function #'treemacs-projectile-create-header)
  :bind (:map global-map
              ("C-c 3" . treemacs-projectile)
              ("C-c 4" . treemacs-projectile-toggle)))

(with-eval-after-load "treemacs"
  (defvar treemacs-custom-html-icon nil)
  (treemacs-define-custom-icon treemacs-custom-html-icon "clj"))

(defun treemacs-header-with-brackets (current-root)
  (format "<%s>" (file-name-nondirectory current-root)))
(setq treemacs-header-function #'treemacs-header-with-brackets)

;; TODO: have it follow version control of a buffer
;; TODO: no icons

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
