;;; Code: https://stackoverflow.com/questions/22971585/switch-parts-of-adjacent-lines#22973948

(defun flip-text-to-eol (&optional up)
  "Flip the text from point to the end of the current line with the text
in the next line from the same column to the end of the next line.

With a prefix arg, flip text with the line above the current."
  (interactive "P")
  (save-excursion
    (let ((tt (delete-and-extract-region (point) (point-at-eol)))
          (c (current-column)))
      (forward-line (if up -1 1))
      (move-to-column c)
      (insert tt)
      (let ((ot (delete-and-extract-region (point) (point-at-eol))))
        (forward-line (if up 1 -1))
        (goto-char (point-at-eol))
        (insert ot)))))

(provide 'flip-text-to-eol)

;;; flip-text-to-eol ends here
