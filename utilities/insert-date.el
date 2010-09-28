;; insert date into buffer at point
;; via spastorino's emacs configuration http://github.com/spastorino
(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %Y-%m-%d - %l:%M %p")))
