; Markdown
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))

; Ruby
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Thorfile$" . ruby-mode))

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

; magit
;; Use auto-fill minor mode within magit-log-edit-mode
;; Set textwidth to 72 characters, keeping with git 
;; best practices (see http://www.tpope.net/node/106)
(setq-default fill-column 72)
(add-hook 'magit-log-edit-mode-hook 'turn-on-auto-fill)

(add-hook 'emacs-lisp-mode-hook 'run-coding-hook)
(add-hook 'css-mode-hook 'run-coding-hook)

(provide 'modes)
