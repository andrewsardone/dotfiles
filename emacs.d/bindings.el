(define-key global-map [(alt return)] 'mac-toggle-max-window)

;; Jump to a definition in the current file.
(global-set-key "\C-x\C-i" 'ido-imenu)

; fuzzy matching
(global-set-key "\C-t" 'fuzzy-find-in-project)

(provide 'bindings)
