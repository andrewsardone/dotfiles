(if (string-equal system-type "darwin")
    (progn
      (setq mac-allow-anti-aliasing t)
      (setq browse-url-browser-function 'browse-url-default-macosx-browser)
      (setq delete-by-moving-to-trash t)
      (set-face-font 'default "-apple-Menlo-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1")))

