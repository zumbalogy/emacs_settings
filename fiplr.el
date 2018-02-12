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
;; https://github.com/grizzl/fiplr/blob/master/fiplr.el

(defun fiplr-list-files-shell-command (type path ignored-globs)
  "Builds the `find' command to locate all project files & directories.
PATH is the base directory to recurse from.
IGNORED-GLOBS is an alist with keys 'DIRECTORIES and 'FILES."
  (let* ((type-abbrev
          (lambda (assoc-type)
            (cl-case assoc-type
              ('directories "d")
              ('files "f"))))
         (name-matcher
          (lambda (glob)
            (mapconcat 'identity
                       `("-name" ,(shell-quote-argument glob))
                       " ")))
         (grouped-name-matchers
          (lambda (type)
            (mapconcat 'identity
                       `(,(shell-quote-argument "(")
                         ,(mapconcat (lambda (v) (funcall name-matcher v))
                                     (cadr (assoc type ignored-globs))
                                     " -o ")
                         ,(shell-quote-argument ")"))
                       " ")))
         (matcher
          (lambda (assoc-type)
            (mapconcat 'identity
                       `(,(shell-quote-argument "(")
                         "-type"
                         ,(funcall type-abbrev assoc-type)
                         ,(funcall grouped-name-matchers assoc-type)
                         ,(shell-quote-argument ")"))
                       " "))))
    (mapconcat 'identity
               `("find"
                 "-L"
                 ,(shell-quote-argument (directory-file-name path))
                 ,(funcall matcher 'directories)
                 "-prune"
                 "-o"
                 "-not"
                 ,(funcall matcher 'files)
                 "-type"
                 ,(funcall type-abbrev type)
                 "-print")
               " ")))

(defun fiplr-list-files (type path ignored-globs)
  "Expands to a flat list of files/directories found under PATH.
The first parameter TYPE is the symbol 'DIRECTORIES or 'FILES."
  (let* ((prefix (file-name-as-directory (file-truename path)))
         (prefix-length (length prefix))
         (list-string
          (shell-command-to-string (fiplr-list-files-shell-command
                                    type
                                    prefix
                                    ignored-globs))))
    (reverse (cl-reduce (lambda (acc file)
                          (if (> (length file) prefix-length)
                              (cons (substring file prefix-length) acc)
                            acc))
                        (split-string list-string "[\r\n]+" t)
                        :initial-value '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun root-dir ()
  (let ((backend (ignore-errors (vc-responsible-backend default-directory))))
    (if backend
        (ignore-errors
          (expand-file-name
           (vc-call-backend backend 'root default-directory)))
      default-directory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq fiplr-ignored-globs
      '((directories (".git" ".svn" ".hg" ".bzr" "target"))
        (files (".#*" "*~" "*.so" "*.o" "*.jpg" "*.png" "*.gif" "*.pdf" "*.gz" "*.zip" "*.elc"))))

(defun list-files ()
  (fiplr-list-files 'files (root-dir) fiplr-ignored-globs))

(defun list-buffers ()
  (mapcar (function buffer-name) (buffer-list)))

(defun list-all ()
  (append recentf-list
          (list-buffers)
          (when (ignore-errors (vc-responsible-backend default-directory))
            (list-files))))

(defun clean-fname (name)
  (let* ((a (string-remove-prefix (root-dir) name))
         (b (string-remove-prefix "home" a))
         (c (string-remove-prefix "" b)))
    c))

(defun list-all-clean ()
  (let* ((empty-filter (lambda (x) (not (string= x ""))))
         (short-names (mapcar 'clean-fname (list-all)))
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
(defun my-find-file (&optional input-fname)
  (interactive)
  (let* ((fname (or input-fname
                    (my-completing-read (list-all-clean)))))
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

(global-unset-key [(control o)])
(global-unset-key [(control t)])
(global-unset-key [(control p)])
(global-set-key [(control o)] 'my-find-file)
(global-set-key [(control t)] 'my-find-file)
(global-set-key [(control p)] 'my-find-file)
