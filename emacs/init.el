(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                      ("marmalade" . "http://marmalade-repo.org/packages/")
;;                      ("melpa" . "http://melpa.org/packages/")))

; list the packages you want
(setq package-list '(
                     typescript
                     seti-theme
                     yaml-mode
                     handlebars-mode
                     nord-theme
                     multi-term
                     monokai-theme
                     exec-path-from-shell
                     dot-mode
                     airline-themes
                     dracula-theme
                     projectile
                     projectile-rails
                     web-mode magit
                     browse-kill-ring
                     ido-vertical-mode
                     flx-ido
                     flx
                     powerline
                     skewer-mode
                     simple-httpd
                     arjen-grey-theme
                     anzu
                     multiple-cursors
                     rspec-mode
                     base16-theme
                     ag
                     cider
                     js2-mode
                     flycheck
                     auto-complete
                     editorconfig
                     )
      )

(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(autoload 'cl "cl")
(autoload 'json "json")

;; Setup
(add-to-list 'load-path (expand-file-name "scripts" user-emacs-directory))

;; enable so long by default
(when (require 'so-long nil :noerror)
  (so-long-enable))

;; Load configs
(require 'init-server)
(require 'init-term)

(require 'init-utils)
(require 'init-line-numbers)
(require 'init-hl-line)
(require 'init-whitespace)
;; (require 'init-editor-config)
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
(require 'init-css)
(require 'init-skewer)

(require 'init-powerline)

(require 'sort-css)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (vue-mode yaml-mode web-mode typescript skewer-mode seti-theme rspec-mode projectile-rails nord-theme multiple-cursors multi-term monokai-theme magit ido-vertical-mode handlebars-mode flycheck flx-ido exec-path-from-shell editorconfig dracula-theme dot-mode cider browse-kill-ring base16-theme auto-complete arjen-grey-theme anzu airline-themes ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
