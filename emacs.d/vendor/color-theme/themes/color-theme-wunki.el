;;; Color theme ispired on Summer Camp theme from Nairda
;;; Ported by Petar Radosevic
(defun color-theme-wunki ()
  "A color theme based on Summer Camp."
  (interactive)
  (color-theme-install
   '(color-theme-wunki
     ((background-color . "#1C1912")
      (background-mode . dark)
      (border-color . "#000000")
      (cursor-color . "#7CDD12")
      (foreground-color . "#DADFB6")
      (mouse-color . "#8ae234"))
     ((help-highlight-face . underline)
      (ibuffer-dired-buffer-face . font-lock-function-name-face)
      (ibuffer-help-buffer-face . font-lock-comment-face)
      (ibuffer-hidden-buffer-face . font-lock-warning-face)
      (ibuffer-occur-match-face . font-lock-warning-face)
      (ibuffer-read-only-buffer-face . font-lock-type-face)
      (ibuffer-special-buffer-face . font-lock-keyword-face)
      (ibuffer-title-face . font-lock-type-face))
     
     
     ;; Mode line
     (border ((t (:background "#FFFFFF"))))
     (fringe ((t (:background "#110F0B"))))
     (mode-line ((t (:foreground "#D7DCB4" :background "#363123"))))
     (mode-line-inactive ((t (:foreground "#D7DCB4" :background "#110F0B"))))

     ;; Standard syntax
     (region ((t (:background "#3E3828"))))
     (font-lock-builtin-face ((t (:foreground "#A2EC4D"))))
     (font-lock-comment-face ((t (:foreground "#93896D"))))
     (font-lock-constant-face ((t (:foreground "#8ae234"))))
     (font-lock-doc-face ((t (:foreground "#93896D"))))
     (font-lock-keyword-face ((t (:foreground "#9EE54B"))))
     (font-lock-string-face ((t (:foreground "#FE9033"))))
     (font-lock-type-face ((t (:foreground "#EB971B"))))
     (font-lock-variable-name-face ((t (:foreground "#EB991D"))))
     (font-lock-warning-face ((t (:foreground "#56A8E4"))))
     (font-lock-function-name-face ((t (:foreground "#579310" :bold t))))
     (show-paren-match-face ((t (:foreground "#9EE54B" :background "#0F0D09"))))
     (show-paren-mismatch-face ((t (:background "#ad7fa8" :foreground "#2e3436"))))
     (minibuffer-prompt ((t (:foreground "#E89315" :bold t))))

     ;; Shell
     (comint-highlight-input ((t (:italic t :bold t))))
     (comint-highlight-prompt ((t (:foreground "#60A312"))))

     ;; I-search
     (isearch ((t (:background "#E79318" :foreground "#FFFFFF"))))
     (isearch-lazy-highlight-face ((t (:foreground "#DCE1B8" :background "#497C0D"))))
     
     ;; Links
     (info-xref ((t (:foreground "#8AE234"))))
     (info-xref-visited ((t (:foreground "#B4DF8A"))))
     
     ;; RestructuredText
     (rst-level-1-face ((t (:foreground "#D7DCB4" :background "#211E16"))))

     ;; Magit
     (magit-item-highlight ((t (:background "#363123"))))
     )))

(provide 'color-theme-wunki)