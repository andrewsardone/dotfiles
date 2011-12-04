;; Base configuration for lisp modes

(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

(defun aps-lisp-coding-hook ()
  "A general lisp coding hook"
  (paredit-mode 1))

(defun aps-interactive-lisp-coding-hook ()
  "A general interactive lisp mode coding hook"
  (paredit-mode 1)
  (prelude-turn-off-whitespace))

(provide 'aps-lisp)
