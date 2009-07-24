(setq load-path (cons "~/.emacs.d/vendor/color-theme-6.6.0" load-path))
;;(add-to-list 'load-path (concat dotfiles-dir "~/emacs.d/vendor/themes"))
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)

(color-theme-railscasts)

(setq mac-allow-anti-aliasing t)

; window size and position
(setq initial-frame-alist
      '((left . 30) (top . 24)
	(width . 145) (height . 50)))

; font
;; The Bitstream Vera Sans Mono font does not come installed
;; To install it, visit http://ftp.gnome.org/pub/GNOME/sources/ttf-bitstream-vera/1.10/
;; Otherwise, uncomment the set-face-font declaration below and comment out the bitstream one
;; (set-face-font 'default "-apple-monaco-medium-r-normal--13-130-72-72-m-130-iso10646-1")
(set-face-font 'default "-apple-Bitstream_Vera_Sans_Mono-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")

(provide 'theme)
