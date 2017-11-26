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
    ("c4bd8fa17f1f1fc088a1153ca676b1e6abc55005e72809ad3aeffb74bd121d23" "3629b62a41f2e5f84006ff14a2247e679745896b5eaa1d5bcfbc904a3441b0cd" "73a13a70fd111a6cd47f3d4be2260b1e4b717dbf635a9caee6442c949fad41cd" default)))
 '(package-selected-packages
   (quote
    (nord-theme multi-term monokai-theme exec-path-from-shell dot-mode airline-themes dracula-theme projectile projectile-rails web-mode magit browse-kill-ring ido-vertical-mode flx-ido flx powerline skewer-mode simple-httpd arjen-grey-theme anzu multiple-cursors rspec-mode base16-theme ag cider js2-mode flycheck auto-complete editorconfig)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "#98ff98")))))

(require 'cl)
(require 'json)
;; Setup

(add-to-list 'load-path (expand-file-name "scripts" user-emacs-directory))

;; Load configs
(require 'init-server)
(require 'init-term)

(require 'init-utils)
(require 'init-line-numbers)
(require 'init-hl-line)
(require 'init-text)
(require 'init-whitespace)
(require 'init-editor-config)
(require 'init-editor-settings)
(require 'init-ido)
(require 'init-keymappings)
(require 'init-dired)

(require 'init-autocomplete)
(require 'init-anzu)
(require 'init-multicursors)
(require 'init-flycheck)
(require 'init-dot-mode)

(require 'init-ruby)
;;(require 'init-rvm)
(require 'init-rspec)

(require 'init-webmode)
(require 'init-javascript)
(require 'init-skewer)

(require 'init-powerline)

(require 'sort-css)
