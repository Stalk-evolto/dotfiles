(define-module (config packages)
  #:use-module (guix packages)
  #:use-module (guix ui)
  #:use-module (ice-9 match)
  #:use-module (guix diagnostics)
  #:export (search-patch
            search-patches
            %patch-path))

(define %distro-root-directory
  ;; Absolute file name of the module hierarchy.  Since (gnu packages …) might
  ;; live in a directory different from (guix), try to get the best match.
  (letrec-syntax ((dirname* (syntax-rules ()
                              ((_ file)
                               (dirname file))
                              ((_ file head tail ...)
                               (dirname (dirname* file tail ...)))))
                  (try
                   (syntax-rules ()
                     ((_ (file things ...) rest ...)
                      (match (search-path %load-path file)
                        (#f
                         (try rest ...))
                        (absolute
                         (dirname* absolute things ...))))
                     ((_)
                      #f))))
    (try ("config/packages/mail.scm" config/ packages/)
         ("config/packages.scm"      config/)
         ("guix.scm"))))

(define %patch-path
  ;; Define it after '%package-module-path' so that '%load-path' contains user
  ;; directories, allowing patches in $GUIX_PACKAGE_PATH to be found.
  (make-parameter
   (map (lambda (directory)
          (if (string=? directory %distro-root-directory)
              (string-append directory "/config/packages/patches")
              directory))
        %load-path)))

(define (search-patch file-name)
  "Search the patch FILE-NAME.  Raise an error if not found."
  (or (search-path (%patch-path) file-name)
      (raise (formatted-message (G_ "~a: patch not found")
                                file-name))))

(define-syntax-rule (search-patches file-name ...)
  "Return the list of absolute file names corresponding to each
FILE-NAME found in %PATCH-PATH."
  (list (search-patch file-name) ...))
