;;; package --- sorting css properties
;;; Commentary:
;;; Code:

(setq sort-css-properties-list '(
                                 "@include"
                                 "@extend"
                                 "display"
                                 "position"
                                 "top"
                                 "right"
                                 "bottom"
                                 "left"
                                 "flex"
                                 "flex-basis"
                                 "flex-direction"
                                 "flex-flow"
                                 "flex-grow"
                                 "flex-shrink"
                                 "flex-wrap"
                                 "align-content"
                                 "align-items"
                                 "align-self"
                                 "justify-content"
                                 "order"
                                 "columns"
                                 "column-gap"
                                 "column-fill"
                                 "column-rule"
                                 "column-span"
                                 "column-count"
                                 "column-width"
                                 "float"
                                 "clear"
                                 "transform"
                                 "transform-origin"
                                 "transition"
                                 "animation"
                                 "animation-name"
                                 "animation-duration"
                                 "animation-timing-function"
                                 "animation-delay"
                                 "animation-iteration-count"
                                 "animation-direction"
                                 "animation-fill-mode"
                                 "animation-play-state"
                                 "visibility"
                                 "opacity"
                                 "z-index"
                                 "margin"
                                 "margin-top"
                                 "margin-right"
                                 "margin-bottom"
                                 "margin-left"
                                 "outline"
                                 "outline-offset"
                                 "outline-width"
                                 "outline-style"
                                 "outline-color"
                                 "border"
                                 "border-top"
                                 "border-right"
                                 "border-bottom"
                                 "border-left"
                                 "border-width"
                                 "border-top-width"
                                 "border-right-width"
                                 "border-bottom-width"
                                 "border-left-width"
                                 "border-style"
                                 "border-top-style"
                                 "border-right-style"
                                 "border-bottom-style"
                                 "border-left-style"
                                 "border-radius"
                                 "border-top-left-radius"
                                 "border-top-right-radius"
                                 "border-bottom-left-radius"
                                 "border-bottom-right-radius"
                                 "border-color"
                                 "border-top-color"
                                 "border-right-color"
                                 "border-bottom-color"
                                 "border-left-color"
                                 "box-shadow"
                                 "background"
                                 "background-attachment"
                                 "background-clip"
                                 "background-color"
                                 "background-image"
                                 "background-repeat"
                                 "background-position"
                                 "background-size"
                                 "cursor"
                                 "padding"
                                 "padding-top"
                                 "padding-right"
                                 "padding-bottom"
                                 "padding-left"
                                 "width"
                                 "min-width"
                                 "max-width"
                                 "height"
                                 "min-height"
                                 "max-height"
                                 "overflow"
                                 "list-style"
                                 "caption-side"
                                 "table-layout"
                                 "border-collapse"
                                 "border-spacing"
                                 "empty-cells"
                                 "vertical-align"
                                 "text-align"
                                 "text-indent"
                                 "text-transform"
                                 "text-decoration"
                                 "text-rendering"
                                 "text-shadow"
                                 "text-overflow"
                                 "line-height"
                                 "word-break"
                                 "word-wrap"
                                 "word-spacing"
                                 "letter-spacing"
                                 "white-space"
                                 "color"
                                 "font"
                                 "font-family"
                                 "font-size"
                                 "font-weight"
                                 "-moz-osx-font-smoothing"
                                 "-webkit-font-smoothing"
                                 "font-smoothing"
                                 "font-style"
                                 "content"
                                 "quotes"
                                 "overflow-x"
                                 "overflow-y"
                                 "backface-visibility"
                                 "box-sizing"
                                 "-webkit-overflow-scrollin"
                                 " "
                                 ""
                                 ))


(defun sort-css-get-property(el)
  (string-match "\[a-z-]*" el)
  (match-string 0 el))

;;; https://emacs.stackexchange.com/questions/32313/custom-sort-order-based-on-certain-elements-of-a-list/32314
(defun sort-css-order (list)
  (sort list
        (lambda (el1 el2)
          (let ((order sort-css-properties-list))
            (< (cl-position (sort-css-get-property (string-trim el1)) order :test 'string=)
               (cl-position (sort-css-get-property (string-trim el2)) order :test 'string=))))))

(defun sort-css-split-and-order (selection)
  (sort-css-order (mapcar 'string-trim (split-string selection "\n"))))

(defun sort-css-modified-selection (selection)
  (mapconcat 'identity (sort-css-split-and-order selection) "\n" ))

(defun sort-css-propertiers(beg end)
  "message region or \"empty string\" if none highlighted"
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-min))))
  (let ((selection (buffer-substring-no-properties beg end)))
    (if (= (length selection) 0)
        (message "nothing selected")
      (replace-string selection (sort-css-modified-selection selection)))))

(provide 'sort-css)
;;; sort-css ends here
