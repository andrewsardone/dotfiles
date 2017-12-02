(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(add-to-list 'exec-path "/usr/local/bin")

;; Don't litter my init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(require 'aps-packages)
(require 'aps-evil)
