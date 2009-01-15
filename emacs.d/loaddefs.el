;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (cheat) "cheat" "elpa-to-submit/cheat.el" (18795
;;;;;;  64217))
;;; Generated autoloads from elpa-to-submit/cheat.el

(autoload (quote cheat) "cheat" "\
Show the specified cheat sheet.

If SILENT is non-nil then do not print any output, but return it
as a string instead.

\(fn NAME &optional SILENT)" t nil)

;;;***

;;;### (autoloads (clojure-mode) "clojure-mode" "elpa-to-submit/clojure-mode.el"
;;;;;;  (18795 64217))
;;; Generated autoloads from elpa-to-submit/clojure-mode.el

(autoload (quote clojure-mode) "clojure-mode" "\
Major mode for editing Clojure code - similar to Lisp mode..
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{clojure-mode-map}
Note that `run-lisp' may be used either to start an inferior Lisp job
or to switch back to an existing one.

Entry to this mode calls the value of `clojure-mode-hook'
if that value is non-nil.

\(fn)" t nil)

(add-to-list (quote auto-mode-alist) (quote ("\\.clj$" . clojure-mode)))

;;;***

;;;### (autoloads (cssh-mode) "cssh" "elpa-to-submit/cssh.el" (18795
;;;;;;  64217))
;;; Generated autoloads from elpa-to-submit/cssh.el

(autoload (quote cssh-mode) "cssh" "\
A major mode for controlling multiple terms at once.

\(fn)" t nil)

;;;***

;;;### (autoloads (gist-fetch gist-buffer gist-region) "gist" "elpa-to-submit/gist.el"
;;;;;;  (18795 64217))
;;; Generated autoloads from elpa-to-submit/gist.el

(autoload (quote gist-region) "gist" "\
Post the current region as a new paste at gist.github.com
Copies the URL into the kill ring.

\(fn BEGIN END)" t nil)

(autoload (quote gist-buffer) "gist" "\
Post the current buffer as a new paste at gist.github.com.
Copies the URL into the kill ring.

\(fn)" t nil)

(autoload (quote gist-fetch) "gist" "\
Fetches a Gist and inserts it into a new buffer
If the Gist already exists in a buffer, switches to it

\(fn ID)" t nil)

;;;***

;;;### (autoloads (run-ruby inf-ruby inf-ruby-keys) "inf-ruby" "elpa-to-submit/inf-ruby.el"
;;;;;;  (18795 64217))
;;; Generated autoloads from elpa-to-submit/inf-ruby.el

(autoload (quote inf-ruby-keys) "inf-ruby" "\
Set local key defs to invoke inf-ruby from ruby-mode.

\(fn)" nil nil)

(autoload (quote inf-ruby) "inf-ruby" "\
Run an inferior Ruby process in a buffer.
With prefix argument, prompts for which Ruby implementation
\(from the list `inf-ruby-implementations') to use. Runs the
hooks `inf-ruby-mode-hook' (after the `comint-mode-hook' is
run).

\(fn &optional IMPL)" t nil)

(autoload (quote run-ruby) "inf-ruby" "\
Run an inferior Ruby process, input and output via buffer *ruby*.
If there is a process already running in `*ruby*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `ruby-program-name').  Runs the hooks `inferior-ruby-mode-hook'
\(after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)

\(fn COMMAND &optional NAME)" t nil)

(eval-after-load (quote ruby-mode) (quote (add-hook (quote ruby-mode-hook) (quote inf-ruby-keys))))

;;;***

;;;### (autoloads (magit-status) "magit" "elpa-to-submit/magit.el"
;;;;;;  (18795 64217))
;;; Generated autoloads from elpa-to-submit/magit.el

(autoload (quote magit-status) "magit" "\
Not documented

\(fn DIR)" t nil)

;;;***

;;;### (autoloads (markdown-mode) "markdown-mode" "elpa-to-submit/markdown-mode.el"
;;;;;;  (18795 64217))
;;; Generated autoloads from elpa-to-submit/markdown-mode.el

(autoload (quote markdown-mode) "markdown-mode" "\
Major mode for editing Markdown files.

\(fn)" t nil)

(add-to-list (quote auto-mode-alist) (quote ("\\.markdown$" . markdown-mode)))

;;;***

;;;### (autoloads (moz-minor-mode) "moz" "elpa-to-submit/moz.el"
;;;;;;  (18795 64217))
;;; Generated autoloads from elpa-to-submit/moz.el

(autoload (quote moz-minor-mode) "moz" "\
Toggle Mozilla mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When Mozilla mode is enabled, some commands become available to
send current code area (as understood by c-mark-function) or
region or buffer to an inferior mozilla process (which will be
started as needed).

\(fn &optional ARG)" t nil)

(eval-after-load (quote js2-mode) (quote (add-hook (quote js2-mode-hook) (quote moz-minor-mode))))

;;;***

;;;### (autoloads (oddmuse-kill-url oddmuse-browse-this-page oddmuse-browse-page
;;;;;;  emacswiki-post oddmuse-insert-pagename oddmuse-revert oddmuse-post
;;;;;;  oddmuse-follow oddmuse-edit oddmuse-toggle-minor) "oddmuse"
;;;;;;  "elpa-to-submit/oddmuse.el" (18795 64217))
;;; Generated autoloads from elpa-to-submit/oddmuse.el

(autoload (quote oddmuse-toggle-minor) "oddmuse" "\
Toggle minor mode state.

\(fn &optional ARG)" t nil)

(autoload (quote oddmuse-edit) "oddmuse" "\
Edit a page on a wiki.
WIKI is the name of the wiki as defined in `oddmuse-wikis',
PAGENAME is the pagename of the page you want to edit.
Use a prefix argument to force a reload of the page.

\(fn WIKI PAGENAME)" t nil)

(autoload (quote oddmuse-follow) "oddmuse" "\
Figure out what page we need to visit
and call `oddmuse-edit' on it.

\(fn ARG)" t nil)

(autoload (quote oddmuse-post) "oddmuse" "\
Post the current buffer to the current wiki.
The current wiki is taken from `oddmuse-wiki'.

\(fn SUMMARY)" t nil)

(autoload (quote oddmuse-revert) "oddmuse" "\
Revert this oddmuse page.

\(fn)" t nil)

(autoload (quote oddmuse-insert-pagename) "oddmuse" "\
Insert a PAGENAME of current wiki with completion.

\(fn PAGENAME)" t nil)

(autoload (quote emacswiki-post) "oddmuse" "\
Post the current buffer to the EmacsWiki.
If this command is invoked interactively: with prefix argument, prompts pagename,
otherwise set pagename as basename of `buffer-file-name'.

This command is intended to post current EmacsLisp program easily.

\(fn &optional PAGENAME SUMMARY)" t nil)

(autoload (quote oddmuse-browse-page) "oddmuse" "\
Ask a WWW browser to load an oddmuse page.
WIKI is the name of the wiki as defined in `oddmuse-wikis',
PAGENAME is the pagename of the page you want to browse.

\(fn WIKI PAGENAME)" t nil)

(autoload (quote oddmuse-browse-this-page) "oddmuse" "\
Ask a WWW browser to load current oddmuse page.

\(fn)" t nil)

(autoload (quote oddmuse-kill-url) "oddmuse" "\
Make the URL of current oddmuse page the latest kill in the kill ring.

\(fn)" t nil)

;;;***

;;;### (autoloads (paredit-mode) "paredit" "elpa-to-submit/paredit.el"
;;;;;;  (18795 64217))
;;; Generated autoloads from elpa-to-submit/paredit.el

(autoload (quote paredit-mode) "paredit" "\
Minor mode for pseudo-structurally editing Lisp code.
\\<paredit-mode-map>

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (ri) "ri" "elpa-to-submit/ri.el" (18795 64217))
;;; Generated autoloads from elpa-to-submit/ri.el

(autoload (quote ri) "ri" "\
Look up Ruby documentation.

\(fn &optional RI-DOCUMENTED)" t nil)

;;;***

;;;### (autoloads (pcomplete/rake ruby-compilation-this-test ruby-compilation-this-buffer
;;;;;;  ruby-compilation-rake) "ruby-compilation" "elpa-to-submit/ruby-compilation.el"
;;;;;;  (18795 64217))
;;; Generated autoloads from elpa-to-submit/ruby-compilation.el

(autoload (quote ruby-compilation-rake) "ruby-compilation" "\
Run a rake process dumping output to a ruby compilation buffer.

\(fn &optional EDIT TASK)" t nil)

(autoload (quote ruby-compilation-this-buffer) "ruby-compilation" "\
Run the current buffer through Ruby compilation.

\(fn)" t nil)

(autoload (quote ruby-compilation-this-test) "ruby-compilation" "\
Run the test at point through Ruby compilation.

\(fn)" t nil)

(autoload (quote pcomplete/rake) "ruby-compilation" "\
Completion rules for the `ssh' command.

\(fn)" nil nil)

;;;***

;;;### (autoloads (color-theme-zenburn) "zenburn" "elpa-to-submit/zenburn.el"
;;;;;;  (18795 64217))
;;; Generated autoloads from elpa-to-submit/zenburn.el

(autoload (quote color-theme-zenburn) "zenburn" "\
Just some alien fruit salad to keep you in the zone.

\(fn)" t nil)

(defalias (quote zenburn) (function color-theme-zenburn))

;;;***

;;;### (autoloads nil nil ("elpa-to-submit/blackboard.el" "elpa-to-submit/color-theme.el"
;;;;;;  "elpa-to-submit/eshell-vc.el") (18795 64221 747127))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; loaddefs.el ends here
