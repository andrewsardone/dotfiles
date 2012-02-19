;; Basic UI configuration, reducing clutter.

;; don't display startup message
(setq inhibit-startup-message t)

;; don't display scratch message
(setq initial-scratch-message nil)

;; no toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode 0))

;; blink cursor
(blink-cursor-mode t)

;; nice scrolling
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Make yes-or-no questions answerable with 'y' or 'n'
(fset 'yes-or-no-p 'y-or-n-p)

;; custom Emacs 24 color themes support
(add-to-list 'custom-theme-load-path (concat aps-root-dir "themes/"))

;; use zenburn as the default theme
(load-theme 'darkclean t)

;; custom color tweaks
(custom-set-faces '(cursor ((t :background "Orange"))))

;; ansi-term colors
(setq ansi-term-color-vector
  [unspecified "#616161" "#ff8277" "#b3fa86" "#fffdc9" "#a6d6fb"
   "#ff90f7" "#d1d2fb" "#f1f1f1"])

(eval-after-load 'diff-mode
  '(custom-set-faces
    '(diff-added ((t (:foreground "#b3fa86"))))
    '(diff-context ((t nil)))
    '(diff-header ((((class color) (min-colors 88) (background dark)) (:foreground "RoyalBlue1"))))
    '(diff-file-header ((((class color) (min-colors 88) (background dark)) (:foreground "RoyalBlue1"))))
    '(diff-function ((t (:foreground "#00bbdd"))))
    '(diff-hunk-header ((t (:foreground "#d1d2fb"))))
    '(diff-nonexistent ((t (:inherit diff-file-header :strike-through nil))))
    '(diff-refine-change ((((class color) (min-colors 88) (background dark)) (:background "#182042"))))
    '(diff-removed ((t (:foreground "#ff8277"))))))

(eval-after-load 'magit
  '(custom-set-faces
     '(magit-diff-add ((t :foreground "#b3fa86")))
     '(magit-diff-del ((t :foreground "#ff8277")))
     '(magit-diff-file-header ((t :foreground "RoyalBlue1")))
     '(magit-diff-hunk-header ((t :foreground "#d1d2fb")))
     '(magit-item-highlight ((t :background "black")))))

;; Mac customizations
(if (string-equal system-type "darwin")
   (progn
     (setq mac-allow-anti-aliasing t)
     (setq browse-url-browser-function 'browse-url-default-macosx-browser)
     (setq delete-by-moving-to-trash t)
     (setq ns-pop-up-frames nil)
     (set-face-font 'default "-apple-Menlo-medium-normal-normal-*-11-*-*-*-m-0-iso10646-1")
     (global-set-key (kbd "s-<return>") 'ns-toggle-fullscreen)))

;; bind shift-{left,right,up,down} to window movement
(windmove-default-keybindings)

(provide 'aps-ui)
