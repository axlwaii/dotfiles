;; auto-complete
;; (autoload 'auto-complete "autoload")
;; (ac-config-default)
;; (set-face-attribute 'ac-candidate-face nil   :background "#111111" :foreground "light gray")
;; (set-face-attribute 'ac-selection-face nil   :background "#333333" :foreground "white")
;; (set-face-attribute 'popup-tip-face    nil   :background "#444444" :foreground "light gray")
(add-hook 'after-init-hook 'global-company-mode)

(provide 'init-company-mode)
