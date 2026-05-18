;;; Directory Local Variables         -*- no-byte-compile: t; -*-
;;; For more information see (info "(emacs) Directory Variables")

((guix-devel-mode
  . ((compile-command
      . (concat "guix system vm --load-path="
                (directory-file-name
                 (file-name-parent-directory
                  (file-name-parent-directory default-directory)))
                " "
                 (if buffer-file-name
                    (shell-quote-argument
                     (buffer-file-name))))))))
