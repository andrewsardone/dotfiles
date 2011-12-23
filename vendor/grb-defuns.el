;; Various definitions stolen from Gary Bernhardt
;;   https://github.com/garybernhardt/dotfiles

; GRB: open temporary buffers in a dedicated window split
(setq grb-temporary-window (nth 2 (window-list)))
(defun grb-special-display (buffer &optional data)
  (let ((window grb-temporary-window))
    (with-selected-window window
      (switch-to-buffer buffer)
      window)))
