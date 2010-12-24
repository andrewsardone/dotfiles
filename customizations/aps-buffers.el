; disable auto-save files (#foo#)
(setq auto-save-default nil)

; disable backup files (foo~)
(setq make-backup-files nil)

; use ibuffer instead of the built in buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

; use browse-kill-ring instead of default yank actions
(browse-kill-ring-default-keybindings)

; pick up changes to files on disk automatically (ie, after git pull)
(global-auto-revert-mode 1)

; Interactively Do Things
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ; case insensitive matching
(add-to-list 'ido-ignore-files "\\.DS_Store")

; default directory
(setq default-directory "~/")
