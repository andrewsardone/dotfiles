;;; init.el

; misc setup
(prefer-coding-system 'utf-8)
(setq make-backup-files nil)
(setq query-replace-highlight t)
(setq search-highlight t)
(setq font-lock-maximum-decoration t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq major-mode 'text-mode)

;; turn on paren matching
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; Get rid of the startup screen
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; no tabs!
(setq-default indent-tabs-mode nil)

(setq default-directory "~/")

(column-number-mode 1)

;; Get rid of toolbar, scrollbar, menubar
(progn
  (when (equal invocation-name '"Emacs")
    (tool-bar-mode 0)
    (scroll-bar-mode 0))
  (menu-bar-mode 0))

(when (equal system-type 'darwin)
  (setenv "PATH" (concat "/opt/local/bin:/usr/local/bin:" (getenv "PATH")))
  (push "/opt/local/bin" exec-path))

; bindings
(define-key global-map [(alt return)] 'mac-toggle-max-window)

;; Jump to a definition in the current file.
(global-set-key "\C-x\C-i" 'ido-imenu)

; fuzzy matching / file finding
(global-set-key "\M-f" 'fuzzy-find-in-project)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "C-'") 'dabbrev-expand)

; random defuns
;; insert date into buffer at point
;; via spastorino's emacs configuration
;; http://github.com/spastorino
(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %Y-%m-%d - %l:%M %p")))

;; Centering code stolen from somewhere and restolen from
;; via spastorino's emacs configuration
;; http://github.com/spastorino
(global-set-key [(control l)]  'centerer)
(defun centerer ()
  "Repositions current line: once middle, twice top, thrice bottom"
  (interactive)
  (cond ((eq last-command 'centerer2)  ; 3 times pressed = bottom
        (recenter -1))
        ((eq last-command 'centerer1)  ; 2 times pressed = top
          (recenter 0)
          (setq this-command 'centerer2))
        (t                             ; 1 time pressed = middle
          (recenter)
          (setq this-command 'centerer1))))

(defun reload-init ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to."
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
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

(defun run-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'coding-hook))

(defun coding-custom ()
  "coding-hook"
  (linum-mode))
(add-hook 'coding-hook '(lambda() (coding-custom)))

; theming
(add-to-list 'load-path "~/.emacs.d/vendor/color-theme")
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(when (equal invocation-name '"Emacs")
  (color-theme-twilight))

(when (equal system-type 'darwin)
  (setq mac-allow-anti-aliasing t))

; window size and position
(setq initial-frame-alist
      '((left . 30) (top . 24)
        (width . 145) (height . 50)))

; font
;; The Bitstream Vera Sans Mono font does not come installed
;; To install it, visit http://ftp.gnome.org/pub/GNOME/sources/ttf-bitstream-vera/1.10/
;; Otherwise, uncomment the set-face-font declaration below and comment out the bitstream one
;; (set-face-font 'default "-apple-monaco-medium-r-normal--13-130-72-72-m-130-iso10646-1")
(set-face-font 'default "-apple-Bitstream_Vera_Sans_Mono-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")

; plugins
;; anything
(add-to-list 'load-path "~/.emacs.d/vendor/anything")
(require 'anything)

;; autotest
(add-to-list 'load-path "~/.emacs.d/vendor/autotest")
(require 'autotest)

;; css
;(add-to-list  'load-path "~/.emacs.d/plugins/css-mode")
;(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
;(setq auto-mode-alist (append '(("\\.css$" . css-mode)) auto-mode-alist))
(add-hook 'css-mode-hook
  (lambda()
  (local-set-key (kbd "<return>") 'newline-and-indent)))

;; DTD mode
(autoload 'dtd-mode "tdtd" "Major mode for SGML and XML DTDs." t)
(autoload 'dtd-etags "tdtd" "Execute etags on FILESPEC and match on DTD-specific regular expressions." t)
(autoload 'dtd-grep "tdtd" "Grep for PATTERN in files matching FILESPEC." t)
(setq auto-mode-alist (append (list
  '("\\.dcl$" . dtd-mode)
  '("\\.dec$" . dtd-mode)
  '("\\.dtd$" . dtd-mode)
  '("\\.ele$" . dtd-mode)
  '("\\.ent$" . dtd-mode)
  '("\\.mod$" . etd-mode))
auto-mode-alist))

;; find-recursive
(add-to-list 'load-path "~/.emacs.d/vendor/find-recursive")
(require 'find-recursive)

;; flymake
(add-to-list  'load-path "~/.emacs.d/vendor/flymake")
(require 'flymake)

(defun flymake-create-temp-intemp (file-name prefix)
  "Return file name in temporary directory for checking FILE-NAME.
This is a replacement for `flymake-create-temp-inplace'. The
difference is that it gives a file name in
`temporary-file-directory' instead of the same directory as
FILE-NAME.

For the use of PREFIX see that function.

Note that not making the temporary file in another directory
\(like here) will not if the file you are checking depends on
relative paths to other files \(for the type of checks flymake
makes)."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((name (concat
                (file-name-nondirectory
                 (file-name-sans-extension file-name))
                "_" prefix))
         (ext  (concat "." (file-name-extension file-name)))
         (temp-name (make-temp-file name nil ext))
         )
    (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
    temp-name))


;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-intemp))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))

(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '(".+\\.rjs$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

(add-hook 'ruby-mode-hook
          '(lambda ()

             ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
             (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
                 (flymake-mode))
             ))

(require 'flymake-jslint)
(add-hook 'javascript-mode-hook
          '(lambda ()
             ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
             (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
                 (flymake-mode))
             ))

;; haml
(add-to-list 'load-path "~/.emacs.d/vendor/haml")
(require 'haml-mode)
(autoload 'haml-mode "haml-mode.el"
  "haml mode for haml files" t)
(setq auto-mode-alist
  (cons '("\\.haml" . haml-mode) auto-mode-alist))

;; javascript
(add-to-list  'load-path "~/.emacs.d/plugins/javascript")
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
(autoload 'javascript-mode "javascript" nil t)

(defvar javascript-identifier-regexp "[a-zA-Z0-9.$_]+")

(defun javascript-imenu-create-method-index-1 (class bound)
  (let (result)
    (while (re-search-forward (format "^ +\\(\%s\\): *function" javascript-identifier-regexp) bound t)
      (push (cons (format "%s.%s" class (match-string 1)) (match-beginning 1)) result))
    (nreverse result)))

(defun javascript-imenu-create-method-index()
  (cons "Methods"
        (let (result)
          (dolist (pattern (list (format "\\b\\(%s\\) *= *Class\.create" javascript-identifier-regexp)
                                 (format "\\b\\([A-Z]%s\\) *= *Object.extend(%s"
                                         javascript-identifier-regexp
                                         javascript-identifier-regexp)
                                 (format "^ *Object.extend(\\([A-Z]%s\\)" javascript-identifier-regexp)
                                 (format "\\b\\([A-Z]%s\\) *= *{" javascript-identifier-regexp)))
            (goto-char (point-min))
            (while (re-search-forward pattern (point-max) t)
              (save-excursion
                (condition-case nil
                    (let ((class (replace-regexp-in-string "\.prototype$" "" (match-string 1)))
                          (try 3))
                      (if (eq (char-after) ?\()
                          (down-list))
                      (if (eq (char-before) ?{)
                          (backward-up-list))
                      (forward-list)
                      (while (and (> try 0) (not (eq (char-before) ?})))
                        (forward-list)
                        (decf try))
                      (if (eq (char-before) ?})
                          (let ((bound (point)))
                            (backward-list)
                            (setq result (append result (javascript-imenu-create-method-index-1 class bound))))))
                  (error nil)))))
          (delete-duplicates result :test (lambda (a b) (= (cdr a) (cdr b)))))))

(defun javascript-imenu-create-function-index ()
  (cons "Functions"
         (let (result)
           (dolist (pattern '("\\b\\([[:alnum:].$]+\\) *= *function" "function \\([[:alnum:].]+\\)"))
             (goto-char (point-min))
             (while (re-search-forward pattern (point-max) t)
               (push (cons (match-string 1) (match-beginning 1)) result)))
           (nreverse result))))

(defun javascript-imenu-create-index ()
  (list
   (javascript-imenu-create-function-index)
   (javascript-imenu-create-method-index)))

(add-hook 'javascript-mode-hook
  (lambda ()
    (setq imenu-create-index-function 'javascript-imenu-create-index)
    (local-set-key (kbd "<return>") 'newline-and-indent)
  )
t)

;; Interactively Do Things (highly recommended, but not strictly required)
(require 'ido)
(ido-mode t)

;; magit
(add-to-list 'load-path "~/.emacs.d/vendor/magit")
(require 'magit)
(eval-after-load 'magit
  '(progn
    (set-face-foreground 'magit-diff-add "green3")
    (set-face-foreground 'magit-diff-del "red3")))
(global-set-key "\C-x\C-g" 'magit-status)
;;; Use auto-fill minor mode within magit-log-edit-mode
;;; Set textwidth to 72 characters, keeping with git
;;; best practices (see http://www.tpope.net/node/106)
(setq-default fill-column 72)
(add-hook 'magit-log-edit-mode-hook 'turn-on-auto-fill)


;; markdown
(add-to-list 'load-path "~/.emacs.d/vendor/markdown")
(require 'markdown-mode)
(defun markdown-custom ()
  "markdown-mode-hook"
  (longlines-mode))
(add-hook 'markdown-mode-hook '(lambda() (markdown-custom)))
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))

;; rdebug
(add-to-list 'load-path "~/.emacs.d/vendor/rdebug")
(require 'rdebug)
(setq rdebug-short-key-mode t)

;; ri-emacs
(setq ri-ruby-script (expand-file-name "~/.emacs.d/vendor/ri-emacs/ri-emacs.rb"))
;(autoload 'ri (expand-file-name "~/.emacs.d/vendor/ri-emacs/ri-ruby.el") nil t)
(load "~/.emacs.d/vendor/ri-emacs/ri-ruby.el")

;; Rinari
(add-to-list 'load-path "~/.emacs.d/vendor/rinari")
(require 'rinari)
(setq rinari-tags-file-name "TAGS")

;; ruby-mode
(add-to-list 'load-path "~/.emacs.d/vendor/ruby-mode")
(require 'ruby-mode)
(require 'ruby-electric)
(add-hook 'ruby-mode-hook 'turn-on-font-lock)
(add-to-list 'auto-mode-alist '("\\.rjs$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Thorfile$" . ruby-mode))

;; ruby-mode-hook
(add-hook 'ruby-mode-hook
  (lambda()
    (add-hook 'write-file-functions
      '(lambda()
        (save-excursion
        (untabify (point-min) (point-max))
        (delete-trailing-whitespace))))
    (set (make-local-variable 'indent-tabs-mode) 'nil)
    (set (make-local-variable 'tab-width) 2)
    (imenu-add-to-menubar "IMENU")
    (require 'ruby-electric)
    (ruby-electric-mode t)
    (require 'ruby-block)
    (ruby-block-mode t)
    ;; (local-set-key 'f1 'ri)
    (local-set-key "\M-\C-i" 'ri-ruby-complete-symbol)
    ;; (local-set-key 'f4 'ri-ruby-show-args)
    (define-key ruby-mode-map "\M-\C-o" 'rct-complete-symbol)
    (local-set-key (kbd "<return>") 'newline-and-indent)))

;; ruby-block
(add-to-list 'load-path "~/.emacs.d/vendor/ruby-block")
(require 'ruby-block)

;; ruby electric
(defun try-complete-abbrev (old)
  (if (expand-abbrev) t nil))
(setq hippie-expand-try-functions-list
     '(try-complete-abbrev
   try-complete-file-name
   try-expand-dabbrev))

;; sass
(add-to-list 'load-path "~/.emacs.d/vendor/sass")
(require 'sass-mode)
(autoload 'sass-mode "sass-mode.el"
   "sass mode for sass files" t)
(setq auto-mode-alist
   (cons '("\\.sass" . sass-mode) auto-mode-alist))

;; tabkey2
;(load "~/.emacs.d/plugins/nxhtml/util/tabkey2.el")

(add-to-list 'load-path "~/.emacs.d/vendor/textmate")
(require 'textmate)
(textmate-mode)

;; yaml
(add-to-list 'load-path "~/.emacs.d/vendor/yaml-mode")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; yasnippet
;; http://yasnippet.googlecode.com/svn/trunk/doc/index.html
(add-to-list 'load-path "~/.emacs.d/vendor/yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/vendor/yasnippet/snippets")
(add-to-list 'load-path "~/.emacs.d/vendor/yasnippet-rails/setup.el")

;; auto-complete
(add-to-list 'load-path "~/.emacs.d/vendor/auto-complete")
(require 'auto-complete-config)
;(global-auto-complete-mode t)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
;;     ;; start completion when entered 3 characters
;(setq ac-auto-start 2)
;; Add following code to your .emacs.
;;
;(define-key ac-complete-mode-map "\t" 'ac-complete)
;(define-key ac-complete-mode-map "\r" nil)


;(add-to-list 'load-path "~/.emacs.d/plugins/auto-complete")
; (when (require 'auto-complete nil t)
;   (require 'auto-complete-yasnippet)
;   (require 'auto-complete-ruby)
;   (require 'auto-complete-css)

   (global-auto-complete-mode t)           ;enable global-mode
   (setq ac-auto-start t)                  ;automatically start
   (setq ac-dwim 3)                        ;Do what i mean
   (setq ac-override-local-map nil)        ;don't override local map
;;   (define-key ac-complete-mode-map "\t" 'ac-expand)
;;   (define-key ac-complete-mode-map "\r" 'ac-complete)
;;   (define-key ac-complete-mode-map "\M-n" 'ac-next)
;;   (define-key ac-complete-mode-map "\M-p" 'ac-previous)
   (set-default 'ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer))

   (setq ac-modes
         (append ac-modes
                 '(eshell-mode
                   ;org-mode
                   )))
   ;(add-to-list 'ac-trigger-commands 'org-self-insert-command)

   (add-hook 'emacs-lisp-mode-hook
             (lambda ()
               (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer ac-source-symbols))))

   (add-hook 'eshell-mode-hook
             (lambda ()
               (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-files-in-current-dir ac-source-words-in-buffer))))

   (add-hook 'ruby-mode-hook
             (lambda ()
               (setq ac-omni-completion-sources '(("\\.\\=" ac-source-rcodetools)))));)
