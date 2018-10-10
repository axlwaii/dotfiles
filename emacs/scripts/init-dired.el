;; dired
(require 'dired )

(put 'dired-find-alternate-file 'disabled nil)

(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file

(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory

(setq dired-auto-revert-buffer  (lambda (_dir) (null (cdr dired-subdir-alist))))

(provide 'init-dired)
