(setq load-path (cons "~/.emacs.d/vendor/color-theme-6.6.0" load-path))
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)

;; (load "~/.emacs.d/vendor/color-theme-ir-black")
;; (color-theme-ir-black)

(setq mac-allow-anti-aliasing t)

; window size and position
(setq initial-frame-alist
      '((left . 30) (top . 40)
	(width . 135) (height . 60)))

; font
(set-face-font 'default "-apple-monaco-medium-r-normal--12-120-72-72-m-120-iso10646-1")

(provide 'theme)
