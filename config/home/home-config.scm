;;; GNU Guix --- Functional package management for GNU
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
  #:use-module (gnu system shadow)
  #:use-module (config home services home-channels)
  #:use-module (guix gexp))

(home-environment
 ;; Below is the list of packages that will show up in your
 ;; Home profile, under ~/.guix-home/profile.
 (packages (specifications->packages (list "docker-cli"
					   "emacs"
					   "emacs-geiser"
					   "emacs-geiser-guile"
					   "emacs-pyvenv"
					   "firefox"
					   "font-adobe-source-han-sans:cn"
					   "font-wqy-microhei"
					   "fontconfig"
					   "gimp"
					   "git"
					   "gnupg"
					   "go"
					   "guile"
					   "i2pd"
					   "icedove"
					   "kdenlive"
					   "libreoffice"
					   "make"
					   "obs"
					   "pinentry"
					   "python"
					   "python-virtualenv"
					   "texlive-latex"
					   "virt-manager"
					   "virt-viewer"
					   "vlc"
					   "rust")))

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
			 ("ls" . "ls -p --color=auto")))
	      (bashrc (list (local-file "../../files/.bashrc"
					"bashrc")))
	      (bash-profile (list (local-file "../../files/.bash_profile"
					      "bash_profile")))
	      (environment-variables
	       `(
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
		      (identity-file "/home/stalk/.ssh/id_ed25519"))))))

    ;; Channels append nonguix
    channels-append-service

    ;; Backup my CONFIG file to dotfiles/files.
    (service home-dotfiles-service-type
	     (home-dotfiles-configuration
	      (directories '("../../files"))
	      (excluded '(".*~"
			  ".*\\.swp"
			  "\\.git"
			  "\\.gitignore"
			  "\\.bash.*")))))
   %base-home-services)))
