(defun color-theme-railscasts ()
  (interactive)
  (color-theme-install
   '(color-theme-railscasts
   ((background-color . "#232323")
    (background-mode . dark)
    (cursor-color . "#5A647E")
    (foreground-color . "#E6E1DC"))
   (default ((t (nil))))
   (bold ((t (:bold t))))
   (bold-italic ((t (:italic t :bold t))))
   (font-lock-builtin-face ((t (:foreground "#D0D0FF"))))
   (font-lock-comment-face ((t (:foreground "#BC9458" :italic t))))
   (font-lock-constant-face ((t (:foreground "#6D9CBE"))))
   (font-lock-doc-string-face ((t (:foreground "red")))) ; TODO
   (font-lock-function-name-face ((t (:foreground "#FFC66D"))))
   (font-lock-keyword-face ((t (:foreground "#CC7833"))))
   (font-lock-preprocessor-face ((t (:foreground "#CC7833")))) ; TODO
   (font-lock-reference-face ((t (:foreground "red")))) ; TODO
   (font-lock-string-face ((t (:foreground "#A5C261"))))
   (font-lock-type-face ((t (:foreground "white"))))
   (font-lock-variable-name-face ((t (:foreground "white"))))
   (font-lock-warning-face ((t (:foreground "red")))) ; TODO
   (highlight ((t (:background "red")))) ; TODO
   (italic ((t (:italic t))))
   (underline ((t (:underline t)))))))
