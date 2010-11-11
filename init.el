;;; init.el

; Common Lisp support
(require 'cl)

(push "/usr/local/bin" exec-path)

; setup load path
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/customizations")
(add-to-list 'load-path "~/.emacs.d/utilities")
(add-to-list 'load-path "~/.emacs.d/vendor")

; handy function to load all elisp files in a directory
(load-file "~/.emacs.d/load-directory.el")

; load utilities
(mapcar 'load-directory '("~/.emacs.d/utilities"))

; load vendor modes
; configured in customizations/aps-modes.el
(vendor 'color-theme)
(vendor 'ido-menu)
(vendor 'magit)
(vendor 'markdown-mode)
(vendor 'nav)
(vendor 'php-mode)
(vendor 'textmate)
(vendor 'yaml-mode)

; load third-party modes that the vendor function can't handle
(add-to-list 'load-path "~/.emacs.d/vendor/js2")

; load customizations
; loaded after vendor modes to allow for custom extensions
(mapcar 'load-directory '("~/.emacs.d/customizations"))

; start a server for usage with emacsclient
(add-hook 'after-init-hook 'server-start)
