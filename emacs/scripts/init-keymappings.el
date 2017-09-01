;;; Navigation
(global-set-key (kbd "C-x f") 'find-file)
(global-set-key (kbd "C-x b") 'switch-to-buffer)
(global-set-key (kbd "C-x p") 'projectile-find-file-dwim)
(global-set-key (kbd "C-x d") 'dired)

;;; Editing
(global-set-key (kbd "C-x u") 'undo)
(global-set-key (kbd "C-c c") 'copy-region-as-kill)
(global-set-key (kbd "C-c l") 'comment-line)
(global-set-key (kbd "C-c k") 'kill-whole-line)

(global-set-key (kbd "C-c n") 'next-buffer)
(global-set-key (kbd "C-c p") 'previous-buffer)

(global-set-key (kbd "M-y") 'browse-kill-ring)

(global-set-key (kbd "M-#") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

(provide 'init-keymappings)
