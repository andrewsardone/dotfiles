;; Common Lisp support
(require 'cl)

(require 'imenu)

;; load the stuff I grabbed from the emacs-prelude project
(require 'prelude-defuns)

;; add the first level subfolders of vendor automatically
(prelude-add-subfolders-to-load-path aps-vendor-dir)

;; my custom defuns

(defun aps-create-terminal ()
  "Create a new terminal buffer"
  (interactive)
  (ansi-term "/bin/zsh"))

(defvar aps-mac-pbcopy-cmd "/usr/bin/pbcopy"
  "Path to the pbcopy command")
(defvar aps-mac-pbpaste-cmd "/usr/bin/pbpaste"
  "Path to the pbpaste command")

(defun aps-mac-pbcopy ()
  "Copy the contents of the region into the Mac OS X clipboard."
  (interactive)
  (call-process-region
    (region-beginning) (region-end) aps-mac-pbcopy-cmd nil t t))

(defun aps-mac-pbpaste ()
  "Paste Mac OS X's clipboard into the buffer at point."
  (interactive)
  (call-process aps-mac-pbpaste-cmd nil t t))

;; via Christopher Wellons
;; http://nullprogram.com/blog/2010/05/11/

;; uuid generation
;; via Christopher Wellons
;; http://nullprogram.com/blog/2010/05/11/
(defun aps-uuid-create ()
  "Return a newly generated UUID. This uses a simple hashing of variable data."
  (let ((s (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                        (user-uid)
                        (emacs-pid)
                        (system-name)
                        (user-full-name)
                        user-mail-address
                        (current-time)
                        (emacs-uptime)
                        (garbage-collect)
                        (random)
                        (recent-keys)))))
    (format "%s-%s-3%s-%s-%s"
            (substring s 0 8)
            (substring s 8 12)
            (substring s 13 16)
            (substring s 16 20)
            (substring s 20 32))))

(defun aps-uuid-insert ()
  "Inserts a new UUID at the point."
  (interactive)
  (insert (aps-uuid-create)))

(defmacro allow-line-as-region-for-function (orig-function)
  "Adds an *-or-line version of the given function that
normally requires region arguments.

This code comes from Aquamac's osxkeys.el and is licensed under
the GPL."
`(defun ,(intern (concat (symbol-name orig-function) "-or-line"))
   ()
   ,(format "Like `%s', but acts on the current line if mark is not active."
            orig-function)
   (interactive)
   (if mark-active
       (call-interactively (function ,orig-function))
     (save-excursion
       ;; define a region (temporarily) -- so any C-u prefixes etc. are preserved.
       (beginning-of-line)
       (set-mark (point))
       (end-of-line)
       (call-interactively (function ,orig-function))))))

;; taken from Chris Wanstrath's textmate.el
(defun aps-define-comment-or-uncomment-line ()
  (unless (fboundp 'comment-or-uncomment-region-or-line)
    (allow-line-as-region-for-function comment-or-uncomment-region)))

(provide 'aps-core)
