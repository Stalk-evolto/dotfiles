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
  #:use-module (config home services emacs)
  #:use-module (config home services home-channels)
  #:use-module (config home services llvm)
  #:use-module (config home services monerod)
  #:use-module (config packages eclip)
  #:use-module (config packages machine-learning)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services messaging)
  #:use-module (gnu home services secrets)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services)
  #:use-module (gnu home)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages finance)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages jami)
  #:use-module (gnu packages kde-multimedia)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu system shadow)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (nongnu packages mozilla)
  #:use-module (srfi srfi-1))

;; Emacs compile GNU Guix to use packages.
(define %emacs-base-packages
  (list aspell
        aspell-dict-en
	emacs
        emacs-debbugs
        git
        ripgrep))

(define %emacs-for-guix
  (list emacs-geiser
        emacs-geiser-guile
	emacs-paredit
        guile-3.0-latest
        guile-readline
        guile-colorized
        guile-hall
        guile-fibers
        guile-git))

(define %emacs-for-c
  (list binutils
        gdb
        autoconf
        automake
        libtool
        gnu-make))

(define %emacs-for-python
  (list python-wrapper
	python-lsp-server
        python-black
        python-flake8
        python-jedi
        python-invoke
        python-pycodestyle
        python-pytest-pycodestyle
        python-pylint
        python-yapf))

(define %emacs-for-texinfo
  (list calc
        curl
        font-arphic-ukai
        font-google-noto-serif-cjk
        font-google-noto-serif-cjk-static
        gnu-gettext
        pinentry
        rubber
        texi2html
        texinfo))

(define %llms
  (list llama-cpp-latest))

(define %hack-tools
  (list nmap
        wireshark
        tcpdump))

(home-environment
 ;; Below is the list of packages that will show up in your
 ;; Home profile, under ~/.guix-home/profile.
 (packages
  (append
   (list
    electron-cash
    electrum
    firefox
    `(,font-adobe-source-han-sans "cn")
    font-gnu-freefont
    font-gnu-unifont
    font-wqy-microhei
    fontconfig
    gimp
    gnupg
    graphviz
    jami
    kdenlive
    libreoffice
    monero
    obs
    pinentry
    recutils
    virt-manager
    vlc)
   %emacs-base-packages
   %emacs-for-guix
   %emacs-for-c
   %emacs-for-python
   %emacs-for-texinfo
   %hack-tools
   %llms))

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
                         ("e" . "emacsclient -t")
                         ("ec" . "emacsclient -c")
                         ("update-home" . "guix home reconfigure -L $HOME/dotfiles $HOME/dotfiles/config/home/home-config.scm")))
              (bashrc (list (local-file "../../files/.bashrc" "bashrc")))
              (bash-profile (list (local-file "../../files/.bash_profile"
                                              "bash_profile")))
              (environment-variables
               `(
                 ("VISUAL" . "emacs")
                 ("EDITOR" . "$VISUAL")
                 ("GTK_IM_MODULE" . "ibus")
                 ("QT_IM_MODULE" . "ibus")
                 ("XMODIFILERS" . "@im=ibus")
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
                      (identity-file "/home/stalk/.ssh/id_ed25519"))
                     (openssh-host
                      (name "debian")
                      (host-name "192.168.100.132")
                      (user "test")
                      (port 22)
                      (forward-agent? #t)
                      (identity-file "/home/stalk/.ssh/test_ed25519"))
                     (openssh-host
                      (name "windows")
                      (host-name "192.168.100.177")
                      (user "Link")
                      (port 22)
                      (forward-agent? #t)
                      (identity-file "/home/stalk/.ssh/stalk_ed25519"))))))

    ;; Password massage.
    (service home-himitsu-service-type)
    (service home-himitsu-ssh-service-type)
    (service home-himitsu-secret-service-type)

    (service home-dbus-service-type)

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
    (service home-znc-service-type)

    ;; llama.cpp model server.
    (service home-llama-server-service-type
             (llama-server-configuration
              (threads 6)
              (extra-options '("--cache-type-k" "q4_0"
                               "--cache-type-v" "q4_0"))))

    ;; emacs daemon.
    (service home-emacs-service-type)

    ;; menero daemon.
    (service home-monerod-service-type
             (home-monerod-configuration
              (proxy "127.0.0.1:9050")
              (p2p-bind-ip "127.0.0.1")
              (no-igd? #t)
              (tx-proxy (list
                         (tx-proxy-configuration
                          (type "tor")
                          (address "127.0.0.1")
                          (port 9050))))
              (anonymous-inbound
               (list
                (anonymous-inbound-configuration
                 (address
                  "ddptxpji2ynf6y4eeudcw4qgqtqswzwtmbpr7z5ei3zdehk47qpgnkyd.onion:18084")
                 (host-local "127.0.0.1:18084")))))))

   %base-home-services)))
