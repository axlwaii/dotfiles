;; auto-complete
(require 'auto-complete)
(ac-config-default)
(set-face-attribute 'ac-candidate-face nil   :background "#111111" :foreground "light gray")
(set-face-attribute 'ac-selection-face nil   :background "#333333" :foreground "white")
(set-face-attribute 'popup-tip-face    nil   :background "#444444" :foreground "light gray")

(provide 'init-autocomplete)
