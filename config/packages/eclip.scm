(define-module (config packages eclip)
  #:use-module (guix packages)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages gdb)
  #:export (emacs-eclip))

(define-public emacs-eclip
(package
  (name "emacs-eclip")
  (version "1.0.1-alpha")
  (source
   (origin
     (method git-fetch)
     (uri (git-reference
	   (url "https://github.com/Stalk-evolto/eclip.git")
	   (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0cirhd51xv5gxhmzalnz4pr04da1yhjhknf2pnn0sgnn11ijhn95"))))
  (native-inputs
   (list emacs))

  (propagated-inputs
   (list emacs-debbugs
	 emacs-guix
	 emacs-geiser
	 emacs-geiser-guile
         emacs-ellama
         emacs-telega
	 emacs-paredit
	 emacs-oauth2
	 emacs-yaml
	 guile-3.0-latest
	 guile-readline
	 guile-colorized
	 gcc-toolchain
	 gdb
	 clang))
  (build-system emacs-build-system)
  (arguments
   (list
    #:include #~(cons* "^core/" "^modules/" "^personal/" %default-include)))
  (properties '((release-tag-prefix . "^v")
                (release-tag-version-delimiter . ".")
                (accept-pre-releases . #t)))
  (synopsis "Emacs config files.")
  (description "Eclip is Emacs config files.")
  (home-page "https://stalk-evolto.github.io")
  (license license:gpl3)))
