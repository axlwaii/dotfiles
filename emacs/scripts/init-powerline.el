(column-number-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COLORS
;;;
;;; Using the NORD - Theme
;;; https://github.com/arcticicestudio/nord/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst nord-polar-night-palette '("#2E3440" "#3B4252" "#434C5E" "#4C566A"))
(defconst nord-frost-palette       '("#8FBCBB" "#88C0D0" "#81A1C1" "#5E81AC"))

(defvar main-bg-color      (nth 2 nord-polar-night-palette))
(defvar secondary-bg-color (nth 3 nord-polar-night-palette))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defface mode-line-directory
  '((t :background "#4C566A" :foreground "#ccc"))
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
              (line-number-mode (" L: %l" (column-number-mode ":%c ")))))

(display-time-mode t)
(timeclock-mode-line-display)

;; make and set faces
(make-face 'mode-line-mode-face)
(make-face 'mode-line-position-face)
(set-face-attribute 'mode-line-mode-face nil :foreground "#98FFCC")
(set-face-attribute 'mode-line-position-face nil :background secondary-bg-color :foreground "white")

(set-face-attribute 'mode-line           nil :background main-bg-color)
(set-face-attribute 'mode-line-buffer-id nil :background secondary-bg-color :foreground "white")

(set-face-attribute 'mode-line-highlight nil :box "#fff" :background "#999")
(set-face-attribute 'mode-line-inactive  nil :inherit 'default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode-line alignment helper
;; thanks to https://emacs.stackexchange.com/questions/16654/how-to-re-arrange-things-in-mode-line

(defun mode-line-fill-right (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
              'face face))


(defun mode-line-fill-center (face reserve)
  "Return empty space using FACE to the center of remaining space leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ center (.5 . right-margin)) ,reserve
                                             (.5 . left-margin))))
              'face face))


(defconst RIGHT_PADDING 1)

(defun reserve-left/middle ()
  (/ (length (format-mode-line mode-line-align-middle)) 2))

(defun reserve-middle/right ()
  (+ RIGHT_PADDING (length (format-mode-line mode-line-align-right))))

;; modelines by alignment

(setq mode-line-align-left
      '(""
        " "
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
        " "))

(setq mode-line-align-middle
      '(""
        ))

(setq mode-line-align-right
      '(""
        (:propertize mode-line-position
                     face mode-line-position-face)
        " "
        mode-line-misc-info
        mode-line-end-spaces
        ))

;; set modeline

(setq-default mode-line-format
              (list
               mode-line-align-left
               '(:eval (mode-line-fill-center 'mode-line
                                              (reserve-left/middle)))
               mode-line-align-middle
               '(:eval
                 (mode-line-fill-right 'mode-line
                                       (reserve-middle/right)))
               mode-line-align-right
               ))

(provide 'init-powerline)
