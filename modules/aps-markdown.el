;; markdown-mode configuration

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

(mapcar
 (lambda (m)
   (add-to-list 'auto-mode-alist `(,m . markdown-mode)))
 '("\\.markdown$" "\\.mkd$" "\\.md$"))

(defun aps-markdown-mode-hook ()
  "A hook for markdown mode"
  (longlines-mode t))

(add-hook 'markdown-mode-hook 'aps-markdown-mode-hook)

(provide 'aps-markdown)
