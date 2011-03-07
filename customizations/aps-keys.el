; magit
(global-set-key (kbd "C-c g") 'magit-status)

; Make yes-or-no questions answerable with 'y' or 'n'
(fset 'yes-or-no-p 'y-or-n-p)

; disable confirmation if a file or buffer does not exist
(setq confirm-nonexistent-file-or-buffer nil)

; disable prompt that asks to create a new, non-existent buffer
(setq ido-create-new-buffer 'always)

; backward-kill-word mapped to C-w (like bash)
; http://steve.yegge.googlepages.com/effective-emacs#item3
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

; personal key-bindings, using C-z as a prefix

; First, unbind C-z so it doesn't minimize the window
; (I'm constantly hitting this binding by accident)
(global-unset-key "\C-z")

(global-set-key (kbd "C-z b") 'browse-url-at-point)

(global-set-key (kbd "C-z R") 'rename-buffer)

(global-unset-key (kbd "C-z t"))
(defun create-terminal ()
  "Create a new terminal buffer"
  (interactive)
  (ansi-term "/bin/zsh"))
(global-set-key (kbd "C-z t") 'create-terminal)

(defun sane-coding-return-key (keymap)
  "Bind RET to newline-and-indent within the KEYMAP keymap. 
Also sets M-RET to simply newline (this mimics TextMate)."
  (define-key keymap (kbd "<return>") 'newline-and-indent)
  (define-key keymap (kbd "M-<return>") 'newline))


