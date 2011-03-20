; Textmate
;; (require 'textmate)
;; (texmate-mode)

; CoffeeScript
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

; CSS
(setq css-indent-offset 2)

; Git
(require 'magit)
(autoload 'magit-status "magit" nil t)
(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)

; ielm
(add-hook 'ielm-mode-hook 
  (lambda () 
    (local-set-key (kbd "<up>") 'comint-previous-input)
    (local-set-key (kbd "<down>") 'comint-next-input)))

; Nav
(require 'nav)

; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

; Emacs Lisp
(defun aps-emacs-lisp-mode-hook ()
  (sane-coding-return-key emacs-lisp-mode-map))
(add-hook 'emacs-lisp-mode-hook 'aps-emacs-lisp-mode-hook)

; JavaScript
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(defun aps-js-mode-hook ()
  (setq js-indent-level 2)
  (sane-coding-return-key js-mode-map)
  (message "APS js-mode hook"))
(add-hook 'js-mode-hook 'aps-js-mode-hook)

; Ruby
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(setq auto-mode-alist (cons '("Rakefile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Capfile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rake" . ruby-mode) auto-mode-alist))
(defun aps-ruby-mode-hook ()
  (sane-coding-return-key ruby-mode-map))
(add-hook 'ruby-mode-hook 'aps-ruby-mode-hook)

; RVM
(require 'rvm)
(rvm-use-default)

; Rails
;; (require 'rinari)
;; (setq rinari-tags-file-name "TAGS")

; Markdown
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-hook 'markdown-mode-hook 
  (lambda () 
    (longlines-mode t)
    (flyspell-mode t)))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

; Mustache
(require 'mustache-mode)
