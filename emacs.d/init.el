;;; init.el

; load paths
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)
(setq custom-file (concat dotfiles-dir "custom.el"))
(setq vendor-dir (concat dotfiles-dir "vendor"))
(setq ext-dir (concat dotfiles-dir "ext"))
(add-to-list 'load-path vendor-dir)

(require 'global)
(require 'defuns)
(require 'bindings)
(require 'modes)
(require 'theme)

;; Language specific stuff
(require 'ruby)
(require 'misc)

;;; Installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;; Interactively Do Things (highly recommended, but not strictly required)
(require 'ido)
(ido-mode t)

;;mode-compile
(autoload 'mode-compile "mode-compile"
 "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
 "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)

;; Rinari
(add-to-list 'load-path "~/.emacs.d/vendor/rinari")
(require 'rinari)

;; yaml-mode
(add-to-list 'load-path "~/.emacs.d/vendor/yaml-mode.el")
(require 'yaml-mode)

;; fuzzy-find-in-project
;; When in a new project directory, set the project root:
;;   (fuzzy-find-project-root "~/path/to/project")
(add-to-list 'load-path "~/.emacs.d/vendor/fuzzy-find-in-project")
(require 'fuzzy-find-in-project)

;; yasnippet
;; http://yasnippet.googlecode.com/svn/trunk/doc/index.html
(add-to-list 'load-path "~/.emacs.d/vendor/yasnippet-0.6.0c")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/vendor/yasnippet-0.6.0c/snippets")

;; Obj-C / Obj-J
(add-to-list 'load-path "/path/to/objc-c-mode.el")
(add-to-list 'load-path "/path/to/objj-mode.el")
(require 'objj-mode)

;; anything
(add-to-list 'load-path "~/.emacs.d/vendor/anything.el")
(require 'anything)

;; ri-emacs
(setq ri-ruby-script (expand-file-name "~/.emacs.d/vendor/ri-emacs/ri-emacs.rb"))
;(load "~/.emacs.d/vendor/ri-emacs/ri-ruby.el")
(autoload 'ri (expand-file-name "~/.emacs.d/vendor/ri-emacs/ri-ruby.el") nil t)

;; cucumber-mode
(add-to-list 'load-path "~/.emacs.d/vendor/cucumber-mode/feature-mode")
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;; rspec-mode
(add-to-list 'load-path "~/.emacs.d/vendor/rspec-mode")
(require 'rspec-mode)
(add-to-list 'auto-mode-alist '("\\_spec.rb$" . rspec-mode))

(provide 'init)

