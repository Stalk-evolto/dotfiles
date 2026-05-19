;;  guix.scm --- Pakcgae define.
;; Copyright (C) 2026  Stalk Evolto <stalk@stalk-laptop>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages gdb)
  #:export (emacs-eclip))

(define-public emacs-eclip
(package
  (name "emacs-eclip")
  (version "1.0.1-rc")
  (source
   (origin
     (method git-fetch)
     (uri (git-reference
	   (url "https://github.com/Stalk-evolto/eclip.git")
	   (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0d60sr5qsyi4210f6rcjh0fxg5bqsv1yihzdr7rm47p4d0d5a8r8"))))
  (native-inputs
   (list emacs))

  (propagated-inputs
   (list emacs-debbugs
	 emacs-guix
	 emacs-geiser
	 emacs-geiser-guile
         emacs-ellama
         emacs-telega
         emacs-auctex
         emacs-cdlatex
	 emacs-paredit
	 emacs-oauth2
	 emacs-yaml
         emacs-markdown-mode
         emacs-nftables-mode
         python-markdown
	 guile-3.0-latest
	 guile-readline
	 guile-colorized
	 gcc-toolchain
	 gdb
	 clang
         python-lsp-server))
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
