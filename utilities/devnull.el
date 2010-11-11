(defun devnull ()
  "Insert '> /dev/null 2>&1'
Handy for keeping your shell scripting quiet."
  (interactive)
  (insert "> /dev/null 2>&1"))
