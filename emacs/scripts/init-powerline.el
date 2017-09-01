(column-number-mode 1)
;; groups

(defface mode-line-directory
  '((t :background "#444" :foreground "#999"))
  "Face used for buffer identification parts of the mode line."
  :group 'mode-line-faces
  :group 'basic-faces)

(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
               (output ""))
       (when (and path (equal "" (car path)))
         (setq path (cdr path)))
       (while (and path (< (length output) (- max-length 4)))
         (setq output (concat (car path) "/" output))
         (setq path (cdr path)))
       (when path
         (setq output (concat ".../" output)))
       output))

(defvar mode-line-directory
  '(:propertize
    (:eval (if (buffer-file-name) (concat "  " (shorten-directory default-directory 25)) " "))
                face mode-line-directory)
  "Formats the current directory.")
(put 'mode-line-directory 'risky-local-variable t)

(setq-default mode-line-buffer-identification
  (propertized-buffer-identification "%b "))

(setq mode-line-position
            '(;; %p print percent of buffer above top of window, o Top, Bot or All
              ;; (-3 "%p")
              ;; %I print the size of the buffer, with kmG etc
              ;; (size-indication-mode ("/" (-4 "%I")))
              ;; " "
              ;; %l print the current line number
              ;; %c print the current column
              (line-number-mode ("  %l" (column-number-mode ":%c ")))))

;; set modeline

(setq-default mode-line-format
      '("%e"
        mode-line-front-space
        ;; mode-line-mule-info
        mode-line-client
        mode-line-modified

        " "
        ;; display mode of file
        ;; mode-line-mode
        (:propertize mode-name
                     face mode-line-mode-face)
        " "
        " "
        (vc-mode vc-mode)
        ;;language-info-alist
        ;; mode-line-remote -- no need to indicate this specially
        " "
        mode-line-directory
        mode-line-buffer-identification
        " "
        (flycheck-mode flycheck-mode-line)
        " "
        mode-line-misc-info
        (:propertize mode-line-position
                     face mode-line-position-face)

        mode-line-end-spaces))

;; make and set faces
(make-face 'mode-line-mode-face)
(make-face 'mode-line-position-face)
(set-face-attribute 'mode-line-mode-face nil :foreground "#98FFCC")
(set-face-attribute 'mode-line-position-face nil :background "#444" :foreground "white")

(set-face-attribute 'mode-line           nil :background "#333")
(set-face-attribute 'mode-line-buffer-id nil :background "#444" :foreground "white")

(set-face-attribute 'mode-line-highlight nil :box nil :background "#999")
(set-face-attribute 'mode-line-inactive  nil :inherit 'default)


(provide 'init-powerline)
