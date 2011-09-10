; Textmate
;; (require 'textmate)
;; (texmate-mode)

; ansi-term

(setq ansi-term-color-vector
  [unspecified "#616161" "#ff8277" "#b3fa86" "#fffdc9" "#a6d6fb"
   "#ff90f7" "#d1d2fb" "#f1f1f1"])

; CoffeeScript
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

; CSS
(setq css-indent-offset 2)

; Git
(autoload 'magit-status "magit" nil t)
(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)

; http-twiddle
(autoload 'http-twiddle-mode "http-twiddle" nil t)
(defun aps-http-twiddle-mode-hook ()
  (local-set-key (kbd "C-c C-u") 'uuid-insert))

(add-hook 'http-twiddle-mode-hook 'aps-http-twiddle-mode-hook)

; ielm
(defun aps-ielm-mode-hook ()
  (local-set-key (kbd "<up>") 'comint-previous-input)
  (local-set-key (kbd "<down>") 'comint-next-input))
  
(add-hook 'ielm-mode-hook 'aps-ielm-mode-hook)

; YAML
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

; Emacs Lisp
(defun aps-emacs-lisp-mode-hook ()
  (sane-coding-return-key emacs-lisp-mode-map)
  (turn-on-eldoc-mode)
  (define-key emacs-lisp-mode-map (kbd "C-z C-f f") 'find-function-at-point)
  (define-key emacs-lisp-mode-map (kbd "C-z C-f v") 'find-variable-at-point))

(add-hook 'emacs-lisp-mode-hook 'aps-emacs-lisp-mode-hook)

; JavaScript
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(add-to-list 'load-path "/usr/local/lib/node_modules/jshint-mode")

(defun aps-js-mode-hook ()
  (require 'flymake-jshint)
  (flymake-mode t)
  (setq js-indent-level 2)
  (sane-coding-return-key js-mode-map))

(add-hook 'js-mode-hook 'aps-js-mode-hook)

; prog-mode
; "Major mode for editing programming language source code"
; Global Coding

(defun aps-prog-mode-hook ()
  (linum-mode 1))

(add-hook 'prog-mode-hook 'aps-prog-mode-hook)

; re-builder
(setq reb-re-builder 'string)

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
(autoload 'rvm-use-default "rvm")
(rvm-use-default)

; shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

; Markdown
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

(defun aps-markdown-mode-hook ()
  (longlines-mode t)
  (flyspell-mode t))

(add-hook 'markdown-mode-hook 'aps-markdown-mode-hook)

(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

; narrow-to-region,
;   enable with C-x n n
;   disable with C-x n w
(put 'narrow-to-region 'disabled nil)
