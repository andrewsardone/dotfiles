;; php-mode configuration

(autoload 'php-mode "php-mode.el"
  "Major mode for editing PHP files" t)

(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

(defun aps-php-mode-hook ()
  "A hook for PHP mode"
  (set-variable 'tab-width 4 t)
  (set-variable 'indent-tabs-mode t t))
(add-hook 'php-mode-hook 'aps-php-mode-hook)

(provide 'aps-php)
