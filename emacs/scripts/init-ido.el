;; IDO CONFIG
(require 'ido)
(require 'ido-vertical-mode)
(require 'flx-ido)

(ido-mode 1)
(ido-everywhere 1)
(ido-vertical-mode 1)
(flx-ido-mode 1)

(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(setq ido-ignore-directories '("node_modules" "tmp" ".git"))

;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(provide 'init-ido)
