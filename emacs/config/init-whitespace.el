;; Whitespace
(require 'whitespace)
(setq-default show-trailing-whitespace t)

(defun no-trailing-whitespace ()
  (setq show-trailing-whitespace nil))

(add-hook 'minibuffer-setup-hook
          'no-trailing-whitespace)
(add-hook 'eww-mode-hook
          'no-trailing-whitespace)
(add-hook 'ielm-mode-hook
          'no-trailing-whitespace)
(add-hook 'gdb-mode-hook
          'no-trailing-whitespace)
(add-hook 'help-mode-hook
          'no-trailing-whitespace)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'init-whitespace)
