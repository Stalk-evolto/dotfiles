(define-module (config packages mail)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages base) #:hide (which))
  #:use-module (gnu packages bison)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages language)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix deprecation)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module ((config packages) #:prefix config:)
  #:export (exim-laster
            dovecot-latest))

(define-public exim-latest
  (package
    (name "exim-latest")
    (version "4.99.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "git://localhost/exim.git")
             (commit (string-append "exim-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ddv0p2iayfki7vzk67b3kjc6428lpf8n96nphrj1gg89q2zwc05"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'configure
                 ;; We'd use #:make-flags but the top-level Makefile calls
                 ;; others recursively, so just set all variables this way.
                 (lambda* (#:key outputs inputs #:allow-other-keys)
                   (chdir "src")
                   (substitute* (list "Makefile" "OS/Makefile-Default")
                     (("(RM_COMMAND=).*" all var)
                      (string-append var "rm\n")))
                   (mkdir "Local")
                   (copy-file "src/EDITME" "Local/Makefile")
                   (copy-file "exim_monitor/EDITME" "Local/eximon.conf")
                   (let ((out (assoc-ref outputs "out")))
                     (substitute* "Local/Makefile"
                       (("(BIN_DIRECTORY=).*" all var)
                        (string-append var out "/bin\n"))
                       (("(CONFIGURE_FILE=).*" all var)
                        (string-append var out "/etc/exim.conf\n"))
                       (("(EXIM_USER=).*" all var)
                        (string-append var "nobody\n"))
                       (("(FIXED_NEVER_USERS=).*" all var)
                        (string-append var "\n")) ; no root in build environment
                       (("(COMPRESS_COMMAND=).*" all var)
                        (string-append var (search-input-file inputs "bin/gzip")
                                       "\n"))
                       (("(ZCAT_COMMAND=).*" all var)
                        (string-append var (search-input-file inputs "bin/zcat")
                                       "\n"))
                       (("# (SUPPORT_SOCKS=yes)" all line) line)
                       (("# (SUPPORT_PROXY=yes)" all line) line)
                       (("# (USE_GNUTLS(|_PC)=.*)" all line)
                        (string-append line "\n"))
                       (("# (AUTH_CRAM_MD5=yes)" all line) line)
                       (("# (AUTH_DOVECOT=yes)" all line) line)
                       (("# (AUTH_EXTERNAL=yes)" all line) line)
                       (("# (AUTH_PLAINTEXT=yes)" all line) line)
                       (("# (AUTH_SPA=yes)" all line) line)
                       (("# (AUTH_TLS=yes)" all line) line)
                       (("# (TRANSPORT_LMTP=yes)" all line) line)))))
               (add-before 'build 'fix-sh-file-names
                 (lambda _
                   (substitute* (list "scripts/lookups-Makefile"
                                      "scripts/reversion"
                                      "scripts/drivers-Makefile")
                     (("SHELL=/bin/sh") "SHELL=sh"))
                   (substitute* "scripts/Configure-config.h"
                     (("\\| /bin/sh") "| sh"))
                   (patch-shebang "scripts/Configure-eximon")))
               (add-before 'build 'fix-perl-file-names
                 (lambda _
                   (substitute* (list  "Local/Makefile"
                                       "OS/Makefile-Default")
                     (("PERL_COMMAND=/usr/bin/perl")
                      (string-append "PERL_COMMAND=" #$perl "/bin/perl")))))
               (add-before 'build 'build-reproducibly
                 (lambda _
                   ;; The ‘compilation number’ increments on every build in the
                   ;; same source tree and varies across different (parallel?)
                   ;; builds.  Make it a ‘constant number’ instead.
                   (substitute* "src/version.c"
                     (("#include \"cnumber.h\"") "1"))))
               ;; (add-after 'build 'install-docs
               ;;   (lambda* (#:key outputs #:allow-other-keys)
               ;;     (chdir (string-append #$source "/doc/doc-docbook"))
               ;;     (apply invoke "make" '("EXIM_VER=0" "spec.info"))
               ;;     (install-file "spec.info"
               ;;                   (string-append (assoc-ref outputs "out")
               ;;                                  "/share/doc/"
               ;;                                  #$name "-" #$version))))
               )
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   "INSTALL_ARG=-no_chown"
                   (string-append "EXIM_RELEASE_VERSION=" #$version))
           ;; No ‘check’ target.  The ‘test/’ suite assumes that particular
           ;; build options were (not) used and that it can freely ‘sudo’.
           #:tests? #f))
    (native-inputs
     (list pcre2 perl pkg-config))
    (inputs
     (list bdb-5.3     ; ‘#error Version 6 and later BDB API is not supported’
           bzip2
           gnutls/dane
           gzip
           libnsl
           libxaw
           libxcrypt
           libxt
           perl
           perl-file-fcntllock
           xz))
    (properties '((release-tag-prefix . "^exim-")
                  (release-tag-suffix . "[a-zA-Z_-]*$")
                  (release-tag-version-delimiter . ".")
                  (accept-pre-releases . #t)))
    (home-page "https://www.exim.org/")
    (synopsis
     "Message Transfer Agent (MTA) developed at the University of Cambridge")
    (description
     "Exim is a message transfer agent (MTA) developed at the University of
Cambridge for use on Unix systems connected to the Internet.  In style it is
similar to Smail 3, but its facilities are more general.  There is a great
deal of flexibility in the way mail can be routed, and there are extensive
facilities for checking incoming mail.")
    (license license:gpl2+)))

(define-public dovecot-latest
  (package
    (name "dovecot-latest")
    (version "2.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dovecot/core.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09v7inrswac8j3cnclkn1z1bd47yl4j5pds83aa33mk7i8i1g621"))
       (patches (config:search-patches "dovecot-2.4.3-wget-download.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config
           autoconf
           automake
           gnu-make
           libtool
           gettext-minimal
           bison
           flex
           perl
           python-minimal
           git))

    (inputs
     (list bzip2
           clucene
           icu4c
           libsodium                    ; extra password algorithms
           libstemmer
           libunwind
           libxcrypt
           linux-pam
           lz4
           openssl
           sqlite
           zlib
           `(,zstd "lib")))
    (arguments
     (list
      #:configure-flags #~(list "--sysconfdir=/etc"
                                "--localstatedir=/var"
                                "--with-sqlite"
                                "--without-lua"
                                "--with-moduledir=/usr/lib/dovecot"
                                "--without-shared-libs")
      #:make-flags #~(list "LDFLAGS=-rdynamic")
      #:phases
      #~(modify-phases %standard-phases
        (add-after 'unpack 'patch-file-names
          (lambda _
            (substitute* "src/lib-program-client/test-program-client-local.c"
              (("(/bin/| )cat") (which "cat"))
              (("/bin/echo") (which "echo"))
              (("/bin/false") (which "false"))
              (("/bin/sh") (which "bash"))
              (("head") (which "head"))
              (("sleep") (which "sleep")))
            (substitute* (list "src/lib-smtp/test-bin/sendmail-exit-1.sh"
                               "src/lib-smtp/test-bin/sendmail-success.sh")
              (("cat") (which "cat")))
            (false-if-file-not-found
             (patch-shebang "build-aux/git-abi-version-gen"))))
        (replace 'install
          (lambda* (#:key outputs make-flags #:allow-other-keys)
            ;; The .la files don't like having the moduledir moved.
            (for-each delete-file (find-files "." "\\.la"))
            ;; Simple hack to avoid installing a trivial README in /etc.
            (apply invoke "make" "install" "sysconfdir=/tmp/bogus"
                   (string-append "moduledir=" (assoc-ref outputs "out") "/lib/dovecot")
                   make-flags))))))
    (home-page "https://www.dovecot.org")
    (synopsis "Secure POP3/IMAP server")
    (description
     "Dovecot is a mail server whose major goals are security and reliability.
It supports mbox/Maildir and its own dbox/mdbox formats.")
    ;; Most source files are covered by either lgpl2.1 or expat.  The SHA code
    ;; is covered by a variant of BSD-3, and UnicodeData.txt is covered by the
    ;; Unicode, Inc. License Agreement for Data Files and Software.
    (license (list license:lgpl2.1 license:expat
                   (license:non-copyleft "file://COPYING")))))
