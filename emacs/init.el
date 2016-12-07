;; PACKAGES
;;

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (rspec-mode helm-projectile helm-cmd-t helm base16-theme ag css-comb cider scss-mode smart-mode-line js2-mode flycheck auto-complete editorconfig evil-rails evil))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

(require 'json)

;; BASIC CONFIG
;;

;; we speak utf-8 here
(set-language-environment "utf-8")
(prefer-coding-system 'utf-8)

(set-default-font "DejaVu Sans Mono-14")

;; disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)

;; use spaces instead of tabs
(setq indent-tabs-mode nil)

;; line-numbers
(global-linum-mode t)
(setq linum-format "%4d  ")
(set-face-attribute 'linum nil :background "#222")

;; no magic comments
(setq ruby-insert-encoding-magic-comment nil)

;; from https://www.emacswiki.org/emacs/linum-off.el
(defcustom linum-disabled-modes-list '(eshell-mode wl-summary-mode compilation-mode org-mode text-mode dired-mode doc-view-mode image-mode)
  "* List of modes disabled when global linum mode is on"
  :type '(repeat (sexp :tag "Major mode"))
  :tag " Major modes where linum is disabled: "
  :group 'linum
  )
(defcustom linum-disable-starred-buffers 't
  "* Disable buffers that have stars in them like *Gnu Emacs*"
  :type 'boolean
  :group 'linum)

(defun linum-on ()
  "* When linum is running globally, disable line number in modes defined in `linum-disabled-modes-list'. Changed by linum-off. Also turns off numbering in starred modes like *scratch*"

  (unless (or (minibufferp)
              (member major-mode linum-disabled-modes-list)
              (string-match "*" (buffer-name))
              (> (buffer-size) 3000000)) ;; disable linum on buffer greater than 3MB, otherwise it's unbearably slow
    (linum-mode 1)))

(provide 'linum-off)

;; disable bells
(setq ring-bell-function 'ignore)
;; disable startup splashscreen
(setq inhibit-startup-message t)

;; dired
(setq dired-auto-revert-buffer  (lambda (_dir) (null (cdr dired-subdir-alist))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whitespace
(require 'whitespace)
(setq-default show-trailing-whitespace t)

(defun no-trailing-whitespace ()
  (setq show-trailing-whitespace nil))

(add-hook 'minibuffer-setup-hook
          'no-trailing-whitespace)
(add-hook 'eww-mode-hook
          'no-trailing-whitespace)
(add-hook 'ielm-mode-hook
          'no-trailing-whitespace)
(add-hook 'gdb-mode-hook
          'no-trailing-whitespace)
(add-hook 'help-mode-hook
          'no-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line hightlighting
(require 'hl-line)
(global-hl-line-mode 1)
(set-face-background 'hl-line "#000")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDO CONFIG
(require 'ido)
(ido-mode t)

;; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)

;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; filetype modes
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.es6\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))

(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rspec
(require 'rspec-mode)
(setq-default rspec-use-rvm t)

;; use bash to run tests as adviced
;; https://github.com/pezra/rspec-mode#zsh-and-rvm
(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))

(ad-activate 'rspec-compile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RVM
(require 'rvm)
(rvm-use-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THEME SETTIaNGS
(load-theme 'base16-summerfruit-dark t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck
(require 'flycheck)
(defun parse-jslinter-warning (warning)
  (flycheck-error-new
   :line (1+ (cdr (assoc 'line warning)))
   :column (1+ (cdr (assoc 'column warning)))
   :message (cdr (assoc 'message warning))
   :level 'error))
(defun jslinter-error-parser (output checker buffer)
  (mapcar 'parse-jslinter-warning
          (cdr (assoc 'warnings (aref (json-read-from-string output) 0)))))
(flycheck-define-checker javascript-jslinter
  "A JavaScript syntax and style checker based on JSLinter.
  See URL `https://github.com/tensor5/JSLinter'."
  :command ("~/jslint" "--raw" source)
  :error-parser jslinter-error-parser
  :modes (js-mode js2-mode js3-mode))

(add-hook 'scss-mode-hook 'flycheck-mode)
(add-hook 'js-mode-hook 'flycheck-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; editorconfig
(require 'editorconfig)
(editorconfig-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil-mode
(require 'evil)
(require 'evil-rails)
(evil-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;smart mode line
(require 'smart-mode-line)
(setq sml/theme 'dark)
(setq sml/no-confirm-load-theme t)
(sml/setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete
(require 'auto-complete)
(ac-config-default)
(set-face-attribute 'ac-candidate-face nil   :background "#111111" :foreground "light gray")
(set-face-attribute 'ac-selection-face nil   :background "#333333" :foreground "white")
(set-face-attribute 'popup-tip-face    nil   :background "#444444" :foreground "light gray")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm
(require 'helm)
(require 'helm-config)
(require 'helm-cmd-t)

(define-key evil-normal-state-map ",f" 'helm-projectile-find-file-dwim)
(define-key evil-normal-state-map ",b" 'helm-buffers-list)

(define-key evil-normal-state-map ",n" 'dired)
(define-key evil-normal-state-map ",z" 'suspend-frame)

;; hooks
(defun define-gf ()
  (if (string-equal (file-name-extension (concat "" (buffer-file-name))) "rb")
      (define-key evil-normal-state-map "gf" (kbd ":RGfile"))
    (define-key evil-normal-state-map "gf" (kbd ":ffap"))
    ))

(add-hook 'find-file-hook 'define-gf)

(add-hook 'find-file-hook
          (lambda ()
            (setq default-directory command-line-default-directory)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
