;; we speak utf-8 here
(set-language-environment "utf-8")
(prefer-coding-system 'utf-8)

;; set font
;; (set-default-font "DejaVu Sans Mono 14")

;; use system clipboard
(setq ns-right-option-modifier nil
      ns-left-option-modifier 'meta
      ;;ns-command-modifier 'meta
      x-select-enable-clipboard t)

;; disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)

(setq backup-directory-alist
  `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t)))

(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

;; Save buffer whe out of focus
(defun save-all ()
  (interactive)
  (save-some-buffers t))

(add-hook 'focus-out-hook 'save-all)

;; cleaner buffers
;; Makes *scratch* empty.
(setq initial-scratch-message "")

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; disable bells
(setq ring-bell-function 'ignore)
;; disable startup splashscreen
(setq inhibit-startup-message t)

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

;; enanbel upcase and downcase shortcuts
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

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

;; THEME
;;(load-theme 'badger t)
;;(load-theme 'arjen-grey t)

(if window-system
    (progn (tool-bar-mode -1)
           (scroll-bar-mode -1)
           (load-theme 'misterioso t)
           (menu-bar-mode t))
    (progn (load-theme 'base16-summerfruit-dark t)
           (menu-bar-mode -1)))

(set-cursor-color "#98ff98")

(blink-cursor-mode 1)

;; use load path from shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(provide 'init-editor-settings)
