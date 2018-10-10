;; Rspec
(autoload 'rspec-mode "rspec-mode")
(setq-default rspec-use-rvm t)

;; use bash to run tests as adviced
;; https://github.com/pezra/rspec-mode#zsh-and-rvm
(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))

(ad-activate 'rspec-compile)

(provide 'init-rspec)
