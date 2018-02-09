(require 'fiplr)
(require 'projectile)
(require 'ido)
(require 'cl)

;; TODO: look into this closing treemacs when jumping for some reason

(recentf-mode 1)
(setq recentf-max-menu-items 50
      ;; recentf-save-file (expand-file-name ".recentf" tmp-local-dir)
      recentf-exclude '("[/\\]\\.elpa/" "[/\\]\\.ido\\.last\\'" "[/\\]\\.git/" ".*\\.gz\\'"))

(add-to-list 'recentf-exclude "/$")

(add-hook 'dired-mode-hook '(lambda () (recentf-mode 0)))

(defun track-file ()
  (recentf-add-file default-directory)
  nil)

(add-hook 'kill-buffer-hook 'track-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq fiplr-ignored-globs
      '((directories (".git" ".svn" ".hg" ".bzr" "target"))
        (files (".#*" "*~" "*.so" "*.o" "*.jpg" "*.png" "*.gif" "*.pdf" "*.gz" "*.zip" "*.elc"))))

(defun list-files ()
  (fiplr-list-files 'files (projectile-project-root) fiplr-ignored-globs))

(defun list-buffers ()
  (mapcar (function buffer-name) (buffer-list)))

(defun list-all ()
  (append recentf-list (list-buffers) (list-files)))

(defun list-all-clean ()
  (let* ((root (projectile-project-root))
         (clean-name (lambda (x) (string-remove-prefix root x)))
         (empty-filter (lambda (x) (not (string= x ""))))
         (short-names (mapcar clean-name (list-all)))
         (non-empty-names (seq-filter empty-filter short-names)))
    (delete-dups non-empty-names)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ido-bindings ()
  (define-key ido-completion-map (kbd "TAB") 'minibuffer-complete)
  (define-key ido-completion-map (kbd "C-a") 'move-beginning-of-line))

(defun my-completing-read (choices)
  (let ((ido-setup-hook (cons 'ido-bindings ido-setup-hook))
        (ido-enable-flex-matching t)
        (ido-enable-prefix nil)
        (ido-max-prospects 4)
        (minibuffer-completion-table choices))
    (ido-completing-read "" choices nil nil nil
                         'extended-command-history
                         (car choices))))

(defun my-find-file ()
  (interactive)
  (let* ((fname (my-completing-read (list-all-clean))))
    (when fname
      (let* ((split-name (split-string fname ":"))
             (file-name (pop split-name))
             (line-name (pop split-name))
             (col-name (pop split-name))
             (file? (not (string-match-p "^:" fname)))
             (line? (and line-name (not (string= "" line-name))))
             (col? (and col-name (not (string= "" col-name)))))
        (when file?
          (find-file file-name))
        (when line?
          (goto-line (string-to-number line-name)))
        (when col?
          (move-to-column (string-to-number col-name)))))))

(global-unset-key [(control t)])
(global-set-key [(control t)] 'my-find-file)
