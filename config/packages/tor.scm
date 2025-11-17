(define-module (config packages tor)
  #:use-module (guix packages)
  #:use-module (guix build utils)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages tor)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages version-control))

(define-public tor-latest
  (package
   (inherit tor)
   (name "tor-latest")
   (version "tor-0.4.9.3-alpha")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "git://localhost/tor.git")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0qyvx45cq0nwjidxy7gp66kljlwycz5a8hycz7pb2vg9pzhp7yy7"))))
   (arguments
     (list #:configure-flags
           #~(list "--enable-gpl"
                   "--enable-lzma"
                   "--enable-zstd"
                   "--disable-asciidoc")
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'adjust-torify
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; Record in 'torify' the absolute file name of 'torsocks'.
                   (let ((torsocks (search-input-file
                                    inputs "/bin/torsocks")))
                     (substitute* "contrib/client-tools/torify"
                       (("pathfind torsocks")
                        "true")
                       (("exec torsocks")
                        (string-append "exec " torsocks))))))
               (add-before 'check 'skip-practracker
                 ;; This is a style linter.  It doesn't get to throw fatal errors.
                 (lambda _
                   (setenv "TOR_DISABLE_PRACTRACKER" "set")))
               #$@(if (or (target-x86-64?)
                          (target-x86-32?))
                     '()
                     ;; Work around upstream issues relating to libseccomp,
                     ;; sandboxing and glibc-2.33.  This is similar to the issue
                     ;; the tor-sandbox-i686 patch fixes but for other architectures.
                     ;; https://gitlab.torproject.org/tpo/core/tor/-/issues/40381
                     ;; https://gitlab.torproject.org/tpo/core/tor/-/issues/40599
                     ;; https://gitlab.torproject.org/tpo/core/tor/-/merge_requests/446
                     `((add-before 'check 'adjust-test-suite
                         (lambda _
                           (substitute* "src/test/test_include.sh"
                             ((".*Sandbox 1.*") ""))
                           (substitute* "src/test/test.c"
                             ((".*sandbox_tests.*") "")))))))))
   (native-inputs
    (list autoconf automake pkg-config python python-wrapper perl lua ruby))
   (inputs
    (list git-minimal
          libevent
          libseccomp
          openssl
          torsocks-latest
          xz
          zlib
          `(,zstd "lib")))
   (properties '((release-tag-prefix . "^tor-")
                 (release-tag-suffix . "(-?[a-z]*)?$")
                 (release-tag-version-delimiter . ".")
                 (accept-pre-releases . #t)))))

(define-public torsocks-latest
  (package
   (inherit torsocks)
   (name "torsocks-latest")
   (version "v2.5.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "git://localhost/torsocks.git")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "00y2541mk833lqz0p3bvaq1aywknbgsh6r5dgnksrk6zvzll6vms"))))
   (inputs
    (list libcap))
   (arguments
    (list
     #:configure-flags
     #~'("CFLAGS=-g -O2 -Wno-error=implicit-function-declaration")
     #:phases #~(modify-phases %standard-phases
                  (add-after 'build 'absolutize
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((getcap (search-input-file
                                     inputs "/sbin/getcap")))
                      (substitute* "src/bin/torsocks"
                                   (("getcap=.*")
                                    (string-append "getcap=" getcap "\n")))))))))))

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

tor-latest
