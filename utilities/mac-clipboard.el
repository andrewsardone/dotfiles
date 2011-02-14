(defvar mac-pbcopy-cmd "/usr/bin/pbcopy")

(defvar mac-pbpaste-cmd "/usr/bin/pbpaste")

(defun mac-pbcopy ()
  "Copy the contents of the region into the Mac OS X clipboard."
  (interactive)
  (call-process-region
    (region-beginning) (region-end) mac-pbcopy-cmd nil t t))

(defun mac-pbpaste ()
  "Paste Mac OS X's clipboard into the buffer at point."
  (interactive)
  (call-process mac-pbpaste-cmd nil t t))
