;; we speak utf-8 here
(set-language-environment "utf-8")
(prefer-coding-system 'utf-8)

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

(if window-system
    ((tool-bar-mode -1)
    (scroll-bar-mode -1)))

;; THEME
;;(load-theme 'badger t)
(load-theme 'base16-summerfruit-dark t)
;;(load-theme 'arjen-grey t)

(provide 'init-editor-settings)
