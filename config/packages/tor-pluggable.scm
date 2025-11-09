(define-module (config packages tor-pluggable)
  #:use-module (guix packages)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages golang-web))

(define-public webtunnel
  (package
   (inherit go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-webtunnel)
   (name "webtunnel")
   (version "v0.0.3")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "git://localhost/webtunnel.git")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "092r0f42cgb2ksplszkxvirmvmmlxd8xvdj555gywy0948c7j7qw"))))
   (arguments
    (list
     #:import-path "gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/webtunnel"
     #:phases
     #~(modify-phases %standard-phases
        (replace 'build
                 (lambda* (#:key
                           build-flags
                           skip-build?
                           import-path
                           (parallel-build? #t)
                           (verbosity 1)
                           #:allow-other-keys)
                   "Build the package named by IMPORT-PATH."
                   (let* ((njobs (if parallel-build? (parallel-job-count) 1))
                          ;; Utilizing GOFLAGS for flexible build options passthrough, refer
                          ;; for more examples to online documentation of Golang
                          ;; <https://go.dev/src/cmd/go/testdata/script/goflags.txt>.
                          (goflags (string-join
                                    (list
                                     ;; Print the name of packages (pathes) as they are compiled.
                                     "-v"
                                     ;; Print each command as it is invoked. When enabled, it
                                     ;; generates a lot of noisy logs which makes identifying
                                     ;; build failures harder to determine.
                                     (if (> verbosity 1) "-x" ""))
                                    " ")))
                     (setenv "GOFLAGS" goflags)
                     (setenv "GOMAXPROCS" (number->string njobs)))

                   (with-throw-handler
                       #t
                     (lambda _
                       (if skip-build?
                           (begin
                             (format #t "Build is skipped, no go files in project's root.~%")
                             #t)
                           (apply invoke "go" "install"
                                  ;; Respectively, strip the symbol table and debug
                                  ;; information, and the DWARF symbol table.
                                  "-ldflags=-s -w"
                                  ;; Remove all file system paths from the resulting
                                  ;; executable.  Instead of absolute file system paths, the
                                  ;; recorded file names will begin either a module
                                  ;; path@version (when using modules), or a plain import path
                                  ;; (when using the standard library, or GOPATH).
                                  "-trimpath"
                                  `(,@build-flags
                                    ,@(map (lambda (dir)
                                             (format #f "~a~:[/~;~]~a"
                                                     import-path (string-null? dir) dir))
                                           '("main/client" "main/server"))))))

                     (lambda (key . args)
                       (display (string-append "Building '" import-path "' failed.\n"
                                               "Here are the results of `go env`:\n"))
                       (invoke "go" "env"))))))))
   (properties '((release-tag-prefix . "^v")
                 (release-tag-version-delimiter . ".")
                 (accept-pre-releases . #t)))))

webtunnel
