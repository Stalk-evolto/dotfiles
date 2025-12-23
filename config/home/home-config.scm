;;; home-config --- Functional home configuration for GNU Guix.
;;; Copyright © 2025 Stalk Evolto <stalk-evolto@outlook.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.
;;
;;
;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(define-module (config home home-config)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services messaging)
  #:use-module (gnu system shadow)
  #:use-module (config home services home-channels)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1))

;; Emacs compile GNU Guix to use packages.
(define %emacs-base-packages
  (list "aspell"
        "aspell-dict-en"
        "emacs"
        "emacs-debbugs"
        "git"
        "ripgrep"
        ))

(define %emacs-for-guix
  (list "emacs-guix"
        "emacs-geiser"
        "emacs-geiser-guile"
        "guile"
        "guile-readline"
        "guile-colorized"
        "guile-hall"))

(define %emacs-for-c
  (list "gcc-toolchain"))

(define %emacs-for-python
  (list "python"
        ;; "python-autopep8"
        "python-black"
        "python-flake8"
        "python-jedi"
        "python-pycodestyle"
        "python-pytest-pycodestyle"
        "python-pylint"
        "python-scrapy"
        "python-virtualenv"
        "python-yapf"))

(define %emacs-for-texinfo
  (list "calc"
        "font-arphic-ukai"
        "font-google-noto-serif-cjk"
        "font-google-noto-serif-cjk-static"
        "gettext"
        "rubber"
        "texi2html"
        "texinfo"
        "texlive-chinese-jfm"
        "texlive-collection-langchinese"
        "texlive-digestif"
        "texlive-lshort-chinese"
        "texlive-noto"
        "texlive-notoccite"
        "texlive-notomath"
        "texlive-pdftex"
        "texlive-scheme-basic"
        "texlive-scripts"
        "texlive-texinfo"
        "texlive-xetex"
        "texlive-xetex-bin"
        "texlive-zhspacing"
        "texlive-zhspacing:doc"
        ))

(define %hack-tools
  (list "nmap"
        "hydra"
        "wireshark"
        "tcpdump"))

(home-environment
 ;; Below is the list of packages that will show up in your
 ;; Home profile, under ~/.guix-home/profile.
 (packages
  (specifications->packages
   (append
    (list
     "recutils"
     "docker-cli"
     "firefox"
     "font-adobe-source-han-sans:cn"
     "font-wqy-microhei"
     "fontconfig"
     "gimp"
     "gnupg"
     "go"
     "icedove"
     "jami"
     "kdenlive"
     "libreoffice"
     "make"
     "mariadb"
     "obs"
     "pinentry"
     "virt-manager"
     "virt-viewer"
     "vlc"
     "rust")
    %emacs-base-packages
    %emacs-for-guix
    %emacs-for-c
    %emacs-for-python
    %emacs-for-texinfo
    %hack-tools)))

 ;; Below is the list of Home services.  To search for available
 ;; services, run 'guix home search KEYWORD' in a terminal.
 (services
  (append
   (list
    ;; Uncomment the shell you wish to use for your user:
                                        ;(service home-bash-service-type)
                                        ;(service home-fish-service-type)
                                        ;(service home-zsh-service-type)

    (service home-bash-service-type
             (home-bash-configuration
              (aliases '(("grep" . "grep --color=auto")
                         ("ip" . "ip -color=auto")
                         ("ll" . "ls -l")
                         ("ls" . "ls -p --color=auto")
                         ("update-home" . "guix home reconfigure -L $HOME/dotfiles $HOME/dotfiles/config/home/home-config.scm")))
              (bashrc (list (local-file "../../files/.bashrc"
                                        "bashrc")))
              (bash-profile (list (local-file "../../files/.bash_profile"
                                              "bash_profile")))
              (environment-variables
               `(
                 ("VISUAL" . "emacs")
                 ("EDITOR" . "$VISUAL")
                 ("GTK_IM_MODULE" . "ibus")
                 ("QT_IM_MODULE" . "ibus")
                 ("XMODIFILERS" . "@im=ibus")
                 ("PATH" . "$HOME/.local/bin:$PATH")
                 ))))

    ;; GNU Readline Package CONFIG file .inputrc
    (service home-inputrc-service-type
             (home-inputrc-configuration
              (key-bindings
               `(("Control-l" . "clear-screen")))
              (variables
               `(("bell-style" . "visible")
                 ("colored-completion-prefix" . #t)
                 ("editing-mode" . "emacs")
                 ("show-mode-in-prompt" . #t)))
              (conditional-constructs
               `(
                 ("$if mode=emacs" .
                  ,(home-inputrc-configuration
                    (variables
                     `(("colored-stats" . #t)
                       ("enable-bracketed-paste" . #t)))))
                 ("$else" .
                  ,(home-inputrc-configuration
                    (variables
                     `(("show-all-if-ambiguous" . #t)))))
                 ("$endif" . #t)))))

    ;; Openssh cleint CONFIG file.
    (service home-openssh-service-type
             (home-openssh-configuration
              (hosts
               (list (openssh-host
                      (name "github.com")
                      (forward-agent? #t)
                      (identity-file "/home/stalk/.ssh/id_ed25519"))
                     (openssh-host
                      (name "childhurd")
                      (host-name "localhost")
                      (user "stalk")
                      (port 10022)
                      (forward-agent? #t)
                      (identity-file "/home/stalk/.ssh/id_ed25519"))))))

    ;; Channels append nonguix.
    channels-append-service

    ;; Backup my CONFIG file to dotfiles/files.
    (service home-dotfiles-service-type
             (home-dotfiles-configuration
              (directories '("../../files"))
              (excluded '(".*~"
                          ".*\\.swp"
                          "\\.git"
                          "\\.gitignore"
                          "\\.bash.*"))))

    ;; ZNC IRC bouncer.
    ;; Start service befor use `znc --makeconf`.
    (service home-znc-service-type))

   %base-home-services)))
