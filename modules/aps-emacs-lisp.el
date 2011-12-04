;; Elisp configuration

(require 'aps-lisp)

;; taken from https://github.com/bbatsov/emacs-prelude
(defun prelude-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(defun aps-emacs-lisp-mode-hook ()
  "A mode hook for emacs-lisp-mode"
  (aps-lisp-coding-hook)
  (turn-on-eldoc-mode)
  (prelude-remove-elc-on-save))

(add-hook 'emacs-lisp-mode-hook 'aps-emacs-lisp-mode-hook)

(defun aps-ielm-mode-hook ()
  "A mode hook for ielm mode"
  (aps-interactive-lisp-coding-hook)
  (turn-on-eldoc-mode)
  (local-set-key (kbd "<up>") 'comint-previous-input)
  (local-set-key (kbd "<down>") 'comint-next-input))

(add-hook 'ielm-mode-hook 'aps-ielm-mode-hook)

(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

(provide 'aps-emacs-lisp)
