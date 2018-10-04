(require 'package)

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

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ))

(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

(require 'cl)
(require 'json)
;; Setup
; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

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
