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
(load-file "~/.emacs.d/vendor/twilight-emacs/color-theme-twilight.el")
(load-file "~/.emacs.d/customizations/color-theme-aps.el")
;; First color-theme is for Cocoa emacs, second is for terminal-based emacs
(setq color-theme-choices '(color-theme-twilight color-theme-aps))
(funcall (lambda (cols)
           (let ((color-theme-is-global nil))
             (eval
              (append '(if (window-system))
                      (mapcar (lambda (x) (cons x nil))
                              cols)))))
           color-theme-choices)
(fset 'test-win-sys
      (funcall (lambda (cols)
                 (lexical-let ((cols cols))
                   (lambda (frame)
                     (let ((color-theme-is-global nil))
                       (select-frame frame)
                       (eval
                        (append '(if (window-system frame))
                                (mapcar (lambda (x) (cons x nil))
                                        cols)))))))
                 color-theme-choices ))
(add-hook 'after-make-frame-functions 'test-win-sys)

; colors
(custom-set-faces
 '(flymake-errline ((t :underline "red")))
 '(flymake-warnline ((t :underline "green"))))

; pretty diff-mode
(custom-set-faces
 '(diff-added ((t (:foreground "green4"))))
 '(diff-context ((t nil)))
 '(diff-file-header ((((class color) (min-colors 88) (background dark)) (:foreground "RoyalBlue1"))))
 '(diff-function ((t (:foreground "#00bbdd"))))
 '(diff-header ((((class color) (min-colors 88) (background dark)) (:foreground "RoyalBlue1"))))
 '(diff-hunk-header ((t (:foreground "#fbde2d"))))
 '(diff-nonexistent ((t (:inherit diff-file-header :strike-through nil))))
 '(diff-refine-change ((((class color) (min-colors 88) (background dark)) (:background "#182042"))))
 '(diff-removed ((t (:foreground "#red3")))))

; pretty magit diffs (based on colors for diff-mode above)
(set-face-attribute 'magit-diff-add nil :foreground "green4")
(set-face-attribute 'magit-diff-del nil :foreground "red4")
(set-face-attribute 'magit-diff-file-header nil :foreground "RoyalBlue1")
(set-face-attribute 'magit-diff-hunk-header nil :foreground "#fbde2d")
(set-face-attribute 'magit-item-highlight nil :background "black")
