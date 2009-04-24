(define-key global-map [(alt return)] 'mac-toggle-max-window)

;; Jump to a definition in the current file.
(global-set-key "\C-x\C-i" 'ido-imenu)

; fuzzy matching / file finding
(global-set-key "\M-f" 'fuzzy-find-in-project)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "C-'") 'dabbrev-expand)

;; Git status
(global-set-key "\C-x\C-g" 'magit-status)

(provide 'bindings)
