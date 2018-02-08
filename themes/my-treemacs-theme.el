(deftheme my-treemacs
  "my-treemacs theme")

(custom-theme-set-faces
 'my-treemacs

 '(default ((t (:background "#202025" :foreground "#bbc2cf" :height 124))))
 )

;; autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'my-treemacs)
