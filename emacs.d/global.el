; no splash screen
(setq inhibit-startup-screen t)

; no toolbar
(tool-bar-mode 0)

; OS X Shortcut Keys
(mac-key-mode)

; show column number
(column-number-mode 1)

; disable auto-save files (#foo#)
(setq auto-save-default nil)

; disable backup files (foo~)
(setq backup-inhibited t)

; no bell
(setq ring-bell-function 'ignore)

; set encoding
(prefer-coding-system 'utf-8)

; no tabs
(setq-default indent-tabs-mode nil)


(provide 'global)
