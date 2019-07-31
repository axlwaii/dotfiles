(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

; list the packages you want
(setq package-list '(
                     multi-term
                     exec-path-from-shell
                     dot-mode
                     projectile
                     go-mode
                     rust-mode
                     typescript-mode
                     js2-mode
                     web-mode
                     vue-mode
                     yaml-mode
                     company
                     magit
                     browse-kill-ring
                     ido-vertical-mode
                     flx-ido
                     flx
                     powerline
                     simple-httpd
                     anzu
                     multiple-cursors
                     ag
                     cider
                     flycheck
                     editorconfig
                     arjen-grey-theme
                     base16-theme
                     nord-theme
                     monokai-theme
                     airline-themes
                     dracula-theme
                     seti-theme
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
(require 'init-hl-line)
(require 'init-whitespace)
(require 'init-editor-settings)
(require 'init-ido)
(require 'init-keymappings)
(require 'init-dired)

(require 'init-company-mode)
(require 'init-anzu)
(require 'init-multicursors)
(require 'init-flycheck)
(require 'init-dot-mode)

(require 'init-webmode)
(require 'init-javascript)
(require 'init-css)

(require 'init-editor-config)
(require 'init-line-numbers)
;;(require 'init-powerline)

(require 'sort-css)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (seti-theme dracula-theme airline-themes monokai-theme nord-theme base16-theme arjen-grey-theme editorconfig flycheck cider ag multiple-cursors anzu simple-httpd powerline flx-ido ido-vertical-mode browse-kill-ring magit yaml-mode vue-mode web-mode typescript-mode rust-mode projectile multi-term mmm-mode js2-mode go-mode exec-path-from-shell dot-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
