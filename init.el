(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(defvar aps-root-dir (file-name-directory load-file-name)
  "The root directory of my Emacs configuration.")

(defvar aps-modules-dir (concat aps-root-dir "modules/")
  "The directory where my personal configurations live in a modular,
fashion, reminiscent of Emacs Prelude's setup.")

(add-to-list 'load-path aps-modules-dir)

(add-to-list 'exec-path "/usr/local/bin")

;; Don't litter my init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(require 'aps-packages)
(require 'aps-editor)
(require 'aps-evil)
(require 'aps-ui)
