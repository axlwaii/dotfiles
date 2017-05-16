(require 'package)

(package-initialize)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-replace-threshold 50)
 '(anzu-replace-to-string-separator " => ")
 '(anzu-search-threshold 1000)
 '(custom-safe-themes
   (quote
    ("9b402e9e8f62024b2e7f516465b63a4927028a7055392290600b776e4a5b9905" default)))
 '(package-selected-packages
   (quote
    (css-comb skewer-mode simple-httpd arjen-grey-theme anzu multiple-cursors rspec-mode helm-projectile helm-cmd-t helm base16-theme ag cider scss-mode js2-mode flycheck auto-complete editorconfig)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'json)

;; Setup

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; Load configs

(require 'init-utils)
(require 'init-line-numbers)
(require 'init-hl-line)
(require 'init-text)
(require 'init-whitespace)
(require 'init-editor-config)
(require 'init-editor-settings)
(require 'init-ido)
(require 'init-dired)

(require 'init-helm)
(require 'init-autocomplete)
(require 'init-anzu)
(require 'init-multicursors)
(require 'init-flycheck)

(require 'init-ruby)
(require 'init-rvm)
(require 'init-rspec)

(require 'init-javascript)
(require 'init-scss)
(require 'init-skewer)
