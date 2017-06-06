;; dired
(setq dired-auto-revert-buffer  (lambda (_dir) (null (cdr dired-subdir-alist))))

(provide 'init-dired)
