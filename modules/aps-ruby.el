;; Basic ruby configuration
;;
;; Depends on inf-ruby, yari

(aps-add-to-auto-mode-alist 'ruby-mode '("\\.rake$"
                                         "Rakefile\\'"
                                         "\\.gemspec$"
                                         "\\.ru$"
                                         "Gemfile\\'"
                                         "Guardfile\\'"))

(eval-after-load 'ruby-mode
  '(progn
     (defun aps-ruby-mode-hook ()
       ;; bind yari to the local keymap normally for displaying
       ;; the `info-emacs-manual'
       (local-set-key (kbd "C-h r") 'yari))

     (add-hook 'ruby-mode-hook 'aps-ruby-mode-hook)))

(provide 'aps-ruby)
