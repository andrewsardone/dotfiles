;; Modify PATH to include my local stuff (mostly homebrew)
(if (eq system-type 'darwin)
    (push "/usr/local/bin" exec-path))

(defvar aps-root-dir (file-name-directory load-file-name)
  "The root directory of my Emacs configuration.")

(defvar aps-modules-dir (concat aps-root-dir "modules/")
  "The directory where my personal configurations live in a modular,
fashion, reminiscent of Emacs Prelude's setup.")

(defvar aps-vendor-dir (concat aps-root-dir "vendor/")
  "The vendor directory for third-party packages that aren't available
in ELPA.")

;; setup the load-path
(add-to-list 'load-path aps-modules-dir)
(add-to-list 'load-path aps-vendor-dir)

(require 'aps-packages)
(require 'aps-core)
(require 'aps-ui)
(require 'aps-editor)
(require 'aps-global-keybindings)

(require 'aps-emacs-lisp)
(require 'aps-clojure)
(require 'aps-markdown)
(require 'aps-php)
