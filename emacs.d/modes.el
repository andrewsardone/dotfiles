; Markdown
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))

; Ruby
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))

; haml & sass
(autoload 'haml-mode "haml-mode.el"
   "haml mode for haml files" t)
(setq auto-mode-alist
   (cons '("\\.haml" . haml-mode) auto-mode-alist))
(autoload 'sass-mode "sass-mode.el"
   "sass mode for sass files" t)
(setq auto-mode-alist
   (cons '("\\.sass" . sass-mode) auto-mode-alist))

; YAML
(add-to-list 'auto-mode-alist '("\\.yml" . yaml-mode))

(provide 'modes)
