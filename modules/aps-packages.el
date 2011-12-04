;; Automatic installation of third-party packages.
;;
;; Ideally I want to install packages via ELPA/marmalade, as that's
;; the community's canonical spot. Anything else should be declared and
;; managed via el-get (https://github.com/dimitri/el-get).

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar aps-packages '(clojure-mode markdown-mode paredit zenburn-theme)
  "A list of third-party packages I use and need installed via package.el")

(dolist (p aps-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; el-get

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))

(setq el-get-sources
      '((:name magit
               :after (lambda ()
                        (global-set-key (kbd "C-c g") 'magit-status)))))

(setq prelude-packages
      (append
       '()
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync prelude-packages)

(provide 'aps-packages)
