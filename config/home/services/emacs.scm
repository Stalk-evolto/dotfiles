;;  emacs.scm --- Emacs daemon service define.
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

(define-module (config home services emacs)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages emacs)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system emacs)
  #:use-module (guix records)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (home-emacs-configuration
            home-emacs-configuration?
            emacs-configuration-emacs
            home-emacs-service-type))

(define-record-type* <home-emacs-configuration> home-emacs-configuration
  make-emacs-configuration
  home-emacs-configuration?
  (emacs emacs-configuration-emacs
         (default emacs)))

(define (emacs-shepherd-service config)
  (let* ((emacs (emacs-configuration-emacs config))
         (log-file #~(string-append %user-log-dir "/emacs.log")))
    (list (shepherd-service
           (documentation
            "Emacs server.  Use @code{emacsclient} to connect to it.")
           (provision '(emacs-server))
           (modules '((shepherd support)))
           (start #~(make-forkexec-constructor
                     (list #$(file-append emacs "/bin/emacs")
                           "--fg-daemon=server")
                     #:log-file #$log-file))
           (stop #~(make-kill-destructor))))))

(define home-emacs-service-type
  (service-type (name 'emacs)
                (extensions
                 (list (service-extension
                        home-shepherd-service-type
                        emacs-shepherd-service)))
                (default-value (home-emacs-configuration))
                (description "Emacs daemon.")))
