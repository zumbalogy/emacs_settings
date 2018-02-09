(require 'fiplr)
(require 'projectile)
(require 'ido)
(require 'cl)

;; TODO: maybe have it sort by most recently used by default or something
;; TODO: maybe set it to not move the file when the cursor is beneath the fold of where
;;     the selector minibuffer thing is coming up
;; TODO: look into this closing treemacs when jumping for some reason

(recentf-mode 1)
(setq recentf-max-menu-items 36)

(defun track-file ()
  (recentf-add-file default-directory)
  nil)

(add-hook 'kill-buffer-hook 'track-file)

(setq fiplr-ignored-globs
      '((directories (".git" ".svn" ".hg" ".bzr" "target"))
        (files (".#*" "*~" "*.so" "*.o" "*.jpg" "*.png" "*.gif" "*.pdf" "*.gz" "*.zip" "*.elc"))))

(defun list-files ()
  (fiplr-list-files 'files (projectile-project-root) fiplr-ignored-globs))

(defun list-buffers ()
  (mapcar (function buffer-name) (buffer-list)))

(defun list-all ()
  (delete-dups (append recentf-list (list-buffers) (list-files))))

(defun ido-bindings ()
  (define-key ido-completion-map (kbd "TAB") 'minibuffer-complete)
  (define-key ido-completion-map (kbd "C-a") 'move-beginning-of-line))

(defun my-completing-read (choices initial-input)
  (let ((ido-setup-hook (cons 'ido-bindings ido-setup-hook))
        (ido-enable-flex-matching t)
        (ido-enable-prefix nil)
        (ido-max-prospects 5)
        (minibuffer-completion-table choices))
    (ido-completing-read "" choices nil nil
                         initial-input
                         'extended-command-history
                         (car choices))))

(defun my-find-file ()
  (interactive)
  (let* ((files (mapcar (lambda (x) (cons (file-name-nondirectory x) x)) (list-all)))
         (fname (my-completing-read (list-all) nil)))
    (when fname
      (let* ((split-name (split-string fname ":"))
             (file-name (pop split-name))
             (line-name (pop split-name))
             (col-name (pop split-name))
             (file (cdr (assoc-string file-name files)))
             (file? (not (string-match-p "^:" fname)))
             (line? (and line-name (not (string= "" line-name))))
             (col? (and col-name (not (string= "" col-name)))))
        (when file?
          (find-file file))
        (when line?
          (goto-line (string-to-number line-name)))
        (when col?
          (move-to-column (string-to-number col-name)))))))

(global-unset-key [(control t)])
(global-set-key [(control t)] 'my-find-file)
