; magit
(global-set-key (kbd "C-c g") 'magit-status)

; Make yes-or-no questions answerable with 'y' or 'n'
(fset 'yes-or-no-p 'y-or-n-p)

; backward-kill-word mapped to C-w (like bash)
; http://steve.yegge.googlepages.com/effective-emacs#item3
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

; Unbind C-z so it doesn't minimize the window
; (I'm constantly hitting this binding by accident)
(global-unset-key "\C-z")
