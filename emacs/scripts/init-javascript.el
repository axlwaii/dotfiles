(autoload 'js2-mode "js2-mode")

(add-to-list 'auto-mode-alist '("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.ts?\\'" . typescript-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

(provide 'init-javascript)
