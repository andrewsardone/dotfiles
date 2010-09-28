; no tabs, just spaces!
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

; encoding
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

; line numbering
(require 'linum)
(global-linum-mode 0)
(setq linum-format "%d ") ; space after line number

; show column number in bar
(column-number-mode t)

; highlight URLs in comments/strings
(add-hook 'find-file-hooks 'goto-address-prog-mode)

; show marks as selections
(setq transient-mark-mode t)

; highlight matching parens
(show-paren-mode t)

; highlight incremental search
(setq search-highlight t)

; apply syntax highlighting to all buffers
(global-font-lock-mode t)

; jump to a definition in the current file.
(global-set-key (kbd "C-x C-i") 'ido-imenu)
