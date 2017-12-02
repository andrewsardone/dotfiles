;; Basic UI configuration, reducing clutter.

(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(menu-bar-mode 0)
(if (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(show-paren-mode t)

(setq ring-bell-function 'ignore)

(setq tab-width 4)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(setq linum-format "%3d ")
(global-linum-mode t)

(column-number-mode t)
(size-indication-mode t)

;; Make yes-or-no questions answerable with 'y' or 'n'
(fset 'yes-or-no-p 'y-or-n-p)

;; custom Emacs 24 color themes support
(add-to-list 'custom-theme-load-path (concat aps-root-dir "themes/"))

;; Mac customizations
(if (string-equal system-type "darwin")
   (progn
     (setq mac-allow-anti-aliasing t)
     (setq browse-url-browser-function 'browse-url-default-macosx-browser)
     (setq delete-by-moving-to-trash t)
     (setq ns-pop-up-frames nil)
      (set-face-attribute 'default nil
                :family "Source Code Pro" :height 130 :weight 'normal)
     (global-set-key (kbd "s-<return>") 'ns-toggle-fullscreen)))

;; bind shift-{left,right,up,down} to window movement
(windmove-default-keybindings)

(use-package atom-one-dark-theme
  :ensure t
  :config
  (load-theme 'atom-one-dark t))

(provide 'aps-ui)
