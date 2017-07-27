;; we speak utf-8 here
(set-language-environment "utf-8")
(prefer-coding-system 'utf-8)

;; use system clipboard
(setq x-select-enable-clipboard t)

;; disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

;; make tab key always call a indent command.
(setq-default tab-always-indent t)

;; make tab key call indent command or insert tab character, depending on cursor position
(setq-default tab-always-indent nil)

;; make tab key do indent first then completion.
(setq-default tab-always-indent 'complete)

;; disable bells
(setq ring-bell-function 'ignore)
;; disable startup splashscreen
(setq inhibit-startup-message t)

;; menubar
(menu-bar-mode -1)

;; show parens
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Turn on the search-highlighting feature
(setq search-highlight 1)

;; Case-insensitive searching
(setq-default case-fold-search t)
(setq case-fold-search t)

;; force vertical split windows
(setq split-width-threshold nil)
(setq split-height-threshold 0)

;; aliases
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable mouse support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  )

;; improve garbage collection
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 10000000))

(setq gc-cons-threshold 10000000)

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(recentf-mode 1)
(setq recentf-max-menu-items 15)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; THEME
;;(load-theme 'badger t)
(load-theme 'base16-summerfruit-dark t)
;;(load-theme 'arjen-grey t)

(provide 'init-editor-settings)
