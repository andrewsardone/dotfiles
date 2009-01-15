;; textmate.el --- TextMate minor mode for Emacs

;; Copyright (C) 2008 Chris Wanstrath <chris@ozmm.org>

;; Licensed under the same terms as Emacs.

;; Version: 0.1.0
;; Keywords: textmate osx mac
;; Created: 22 Nov 2008
;; Author: Chris Wanstrath <chris@ozmm.org>

;; This file is NOT part of GNU Emacs.

;; Licensed under the same terms as Emacs.

;;; Commentary:

;; This minor mode exists to mimick TextMate's awesome
;; features.

;;    ⌘T - Go to File
;;  ⇧⌘T - Go to Symbol
;;    ⌘L - Go to Line
;;    ⌘/ - Comment Line (or Selection/Region)
;;    ⌘] - Shift Right (currently indents region)
;;    ⌘[ - Shift Left  (not yet implemented)
;;  ⌥⌘] - Align Assignments
;;  ⌥⌘[ - Indent Line
;;  ⌘RET - Insert Newline at Line's End
;;  ⌥⌘T - Reset File Cache (for Go to File)

;; A "project" in textmate-mode is determined by the presence of
;; a .git directory. If no .git directory is found in your current
;; directory, textmate-mode will traverse upwards until one (or none)
;; is found. The directory housing the .git directory is presumed
;; to be the project's root.

;; In other words, calling Go to File from
;; ~/Projects/fieldrunners/app/views/towers/show.html.erb will use
;; ~/Projects/fieldrunners/ as the root if ~/Projects/fieldrunners/.git
;; exists.

;;; Installation

;; $ cd ~/.emacs.d/vendor
;; $ git clone git://github.com/defunkt/textmate.el.git
;;
;; In your emacs config:
;;
;; (add-to-list 'load-path "~/.emacs.d/vendor/textmate.el")
;; (require 'textmate)
;; (textmate-mode)

;;; Minor mode

(defvar textmate-use-file-cache t
  "* Should `textmate-goto-file' keep a local cache of files?")

(defvar textmate-completing-library 'ido
  "The library `textmade-goto-symbol' and `textmate-goto-file' should use for completing filenames and symbols (`ido' by default)")

(defvar textmate-completing-function-alist '((ido ido-completing-read)
                                             (icicles  icicle-completing-read)
                                             (none completing-read))
  "The function to call to read file names and symbols from the user")

(defvar textmate-completing-minor-mode-alist
  `((ido ,(lambda (a) (progn (ido-mode a) (setq ido-enable-flex-matching t))))
    (icicles ,(lambda (a) (icy-mode a)))
    (none ,(lambda (a) ())))
  "The list of functions to enable and disable completing minor modes")

(defvar textmate-mode-map (make-sparse-keymap))
(defvar *textmate-project-root* nil)
(defvar *textmate-project-files* '())
(defvar *textmate-gf-exclude*
  "/\\.|vendor|fixtures|tmp|log|build|\\.xcodeproj|\\.nib|\\.framework|\\.app|\\.pbproj|\\.pbxproj|\\.xcode|\\.xcodeproj|\\.bundle")

;;; Bindings

(defun textmate-ido-fix ()
  "Add up/down keybindings for ido."
  (define-key ido-completion-map [up] 'ido-prev-match)
  (define-key ido-completion-map [down] 'ido-next-match))

(defun textmate-bind-keys ()
  (add-hook 'ido-setup-hook 'textmate-ido-fix)
  (if (boundp 'aquamacs-version)
      (textmate-bind-aquamacs-keys)
    (textmate-bind-carbon-keys)))

(defun textmate-bind-aquamacs-keys ()
  (define-key textmate-mode-map [A-return] 'textmate-next-line)
  (define-key textmate-mode-map (kbd "A-M-t") 'textmate-clear-cache)
  (define-key textmate-mode-map (kbd "A-M-]") 'align)
  (define-key textmate-mode-map (kbd "A-M-[") 'indent-according-to-mode)
  (define-key textmate-mode-map (kbd "A-]") 'indent-region)
  (define-key textmate-mode-map (kbd "A-/") 'comment-or-uncomment-region-or-line)
  (define-key osx-key-mode-map (kbd "A-t") 'textmate-goto-file) ;; Need `osx-key-mode-map' to override
  (define-key osx-key-mode-map (kbd "A-T") 'textmate-goto-symbol)) ;; Aquamacs menu item key bindings.

(defun textmate-bind-carbon-keys ()
  ;; Are these any good? Anyone have good Carbon defaults?
  (define-key textmate-mode-map [M-return] 'textmate-next-line)
  (define-key textmate-mode-map [C-return] 'textmate-previous-line)
                                        ;  (define-key textmate-mode-map (kbd "A-M-t") 'textmate-clear-cache)
  (define-key textmate-mode-map (kbd "M-[") 'align)
                                        ;  (define-key textmate-mode-map (kbd "A-M-[") 'indent-according-to-mode)
  (define-key textmate-mode-map (kbd "M-=") 'comment-or-uncomment-region-or-line)
  (define-key textmate-mode-map [(control tab)] 'indent-region)
  (define-key textmate-mode-map [(meta t)] 'textmate-goto-file)
  (define-key textmate-mode-map [(meta T)] 'textmate-goto-symbol))

(defun textmate-completing-read (&rest args)
  (let ((reading-fn (cadr (assoc textmate-completing-library textmate-completing-function-alist))))
    (apply (symbol-function reading-fn) args)))

;;; Commands

;; TextMate-like commenting
;; http://paste.lisp.org/display/42657
(defun comment-or-uncomment-line (&optional lines)
  "Comment current line. Argument gives the number of lines
forward to comment"
  (interactive "P")
  (comment-or-uncomment-region
   (line-beginning-position)
   (line-end-position lines)))

(defun comment-or-uncomment-region-or-line (&optional lines)
  "If the line or region is not a comment, comments region
if mark is active, line otherwise. If the line or region
is a comment, uncomment."
  (interactive "P")
  (if mark-active
      (if (< (mark) (point))
          (comment-or-uncomment-region (mark) (point))
        (comment-or-uncomment-region (point) (mark)))
    (comment-or-uncomment-line lines)))


(defun textmate-next-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun textmate-previous-line ()
  (interactive)
  (beginning-of-line)
  (newline-and-indent)
  (previous-line)
  (indent-according-to-mode))

;; http://chopmo.blogspot.com/2008/09/quickly-jumping-to-symbols.html
(defun textmate-goto-symbol ()
  "Will update the imenu index and then use ido to select a symbol to navigate to"
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol (textmate-completing-read "Symbol: " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

(defun textmate-goto-file (&optional starting)
  (interactive)
  (when (null (textmate-set-project-root))
    (error "Can't find any .git directory"))
  (find-file (concat (expand-file-name *textmate-project-root*) "/"
                     (textmate-completing-read "Find file: "
                                               (or (textmate-cached-project-files)
                                                   (textmate-cache-project-files *textmate-project-root*))))))

(defun textmate-clear-cache ()
  (interactive)
  (setq *textmate-project-root* nil)
  (setq *textmate-project-files* nil)
  (message "textmate-mode cache cleared."))

;;; Utilities

(defun textmate-project-files (root)
  (split-string
   (shell-command-to-string
    (concat
     "find "
     root
     " -type f  | grep -vE '"
     *textmate-gf-exclude*
     "' | sed 's:"
     *textmate-project-root*
     "/::'")) "\n" t))

(defun textmate-cache-project-files (root)
  (let ((files (textmate-project-files root)))
    (setq *textmate-project-files* `(,root . ,files))
    files))

(defun textmate-cached-project-files ()
  (when (and
         textmate-use-file-cache
         (not (null *textmate-project-files*))
         (equal *textmate-project-root* (car *textmate-project-files*)))
    (cdr *textmate-project-files*)))

(defun textmate-project-root ()
  (or (textmate-set-project-root) *textmate-project-root*))

(defun textmate-set-project-root ()
  (when (or
         (null *textmate-project-root*)
         (not (string-match *textmate-project-root* default-directory)))
    (let ((root (textmate-find-project-root)))
      (if root
          (setq *textmate-project-root* (expand-file-name (concat root "/")))
        (setq *textmate-project-root* nil))))
  *textmate-project-root*)

(defun textmate-find-project-root (&optional root)
  (when (null root) (setq root default-directory))
  (cond
   ((member ".git" (directory-files root)) (expand-file-name root))
   ((equal (expand-file-name root) "/") nil)
   (t (textmate-find-project-root (concat (file-name-as-directory root) "..")))))

;;;###autoload
(define-minor-mode textmate-mode "TextMate Emulation Minor Mode"
  :lighter " mate" :global t :keymap textmate-mode-map
  (textmate-bind-keys)
                                        ; activate preferred completion library
  (dolist (mode textmate-completing-minor-mode-alist)
    (if (eq (car mode) textmate-completing-library)
        (funcall (cadr mode) t)
      (when (fboundp
             (cadr (assoc (car mode) textmate-completing-function-alist)))
        (funcall (cadr mode) -1)))))

;; Topfunky enhancements. Maybe move to passenger.el file?

(defun passenger-restart (&optional starting)
  (interactive)
  (when (null (textmate-set-project-root))
    (error "Can't find any .git directory"))
  (shell-command-to-string
   (concat
    "touch "
    *textmate-project-root*
    "/tmp/restart.txt")) "\n" t)

(defun regen-ctags (&optional starting)
  (interactive)
  (when (null (textmate-set-project-root))
    (error "Can't find any .git directory"))
  (shell-command-to-string
   (concat
    "rm -f " *textmate-project-root* "/TAGS && "
    "cd " *textmate-project-root* " && "
    "ctags -R -e -f " *textmate-project-root* "TAGS app config lib vendor")) "\n" t)

(provide 'textmate)
;;; textmate.el ends here
