(require 'yasnippet)

(defun yas-truncate-tagname (s)
  (replace-regexp-in-string "\\([^[:blank:]]+\\).*" "\\1" s))

(defun yas-self-closing-tag? (s)
  (s-ends-with? "/" s))
