;; My preferred global keybindings

; backward-kill-word mapped to C-w (like bash)
; http://steve.yegge.googlepages.com/effective-emacs#item3
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "M-i") 'prelude-ido-goto-symbol)

;; File finding
(global-set-key (kbd "C-x f") 'prelude-recentf-ido-find-file)

;; Window switching. (C-x o goes to the next window)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1))) ;; back one

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c e") 'prelude-eval-and-replace)

;; cycle through buffers
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; open in external application
(global-set-key (kbd "C-c o") 'prelude-open-with)

;; handy undo
(global-unset-key (kbd "C-\-"))
(global-set-key (kbd "C-\-") 'undo)

;; comment or uncomment
;; removes the default C-/ keybinding for `undo', but I already
;; use the default C-x u or my custom C--
(global-unset-key (kbd "C-/"))
(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)
(if (eq system-type 'darwin)
    (global-set-key (kbd "s-/") 'comment-or-uncomment-region-or-line))

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

; unbind C-z so it doesn't minimize the window
; (I'm constantly hitting this binding by accident)
(global-unset-key "\C-z")

(global-set-key (kbd "C-z b") 'browse-url-at-point)

(global-set-key (kbd "C-z R") 'rename-buffer)

(global-unset-key (kbd "C-z t"))

(global-set-key (kbd "C-z t") 'aps-create-terminal)

(global-set-key (kbd "C-z T") 'toggle-truncate-lines)

(provide 'aps-global-keybindings)
