; don't display startup message
(setq inhibit-startup-message t)

; don't display scratch message
(setq initial-scratch-message nil)

; no toolbar
(if (fboundp 'toolbar-bar-mode) (toolbar-bar-mode 0))

; no scrollbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

; no menubar
(if (fboundp 'menu-bar-mode) (menu-bar-mode 0))

; blink cursor
(blink-cursor-mode t)

; no bell
(setq ring-bell-function 'ignore)

; theme
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(when (equal invocation-name '"Emacs")
  (color-theme-ir-black))

;; ; colors
;; (custom-set-faces
;;  '(flymake-errline ((t :underline "red")))
;;  '(flymake-warnline ((t :underline "green"))))

;; ; pretty diff-mode
;; (custom-set-faces
;;  '(diff-added ((t (:foreground "#559944"))))
;;  '(diff-context ((t nil)))
;;  '(diff-file-header ((((class color) (min-colors 88) (background dark)) (:foreground "RoyalBlue1"))))
;;  '(diff-function ((t (:foreground "#00bbdd"))))
;;  '(diff-header ((((class color) (min-colors 88) (background dark)) (:foreground "RoyalBlue1"))))
;;  '(diff-hunk-header ((t (:foreground "#fbde2d"))))
;;  '(diff-nonexistent ((t (:inherit diff-file-header :strike-through nil))))
;;  '(diff-refine-change ((((class color) (min-colors 88) (background dark)) (:background "#182042"))))
;;  '(diff-removed ((t (:foreground "#de1923")))))

;; ; pretty magit diffs (based on colors for diff-mode above)
;; (set-face-attribute 'magit-diff-add nil :foreground "#559944")
;; (set-face-attribute 'magit-diff-del nil :foreground "#de1923")
;; (set-face-attribute 'magit-diff-file-header nil :foreground "RoyalBlue1")
;; (set-face-attribute 'magit-diff-hunk-header nil :foreground "#fbde2d")
;; (set-face-attribute 'magit-item-highlight nil :background "black")
