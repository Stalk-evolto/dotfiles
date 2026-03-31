;;; i2pd.scm --- Functional service for GNU Guix.
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

(define-module (config services i2pd)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services admin)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages i2p)
  #:use-module (gnu packages admin)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export (i2pd-configuration i2pd-configuration? i2pd-service-type))

;; I2p configuration default
(define-record-type* <i2pd-configuration> i2pd-configuration
  make-i2pd-configuration
  i2pd-configuration?
  (i2pd i2pd-configuration-i2pd
        (default i2pd))
  (config-file i2pd-configuration-config-file
               (default (plain-file "i2pd.conf" "log = true\nipv6 = true")))
  (pid-file i2pd-configuration-pid-file
            (default "/var/run/i2pd/i2pd.pid"))
  (log-file i2pd-configuration-log-file
            (default "/var/log/i2pd/i2pd.log"))
  (tunnels-config-file i2pd-configuration-tunnels-config-file
                       (default (plain-file "tunnels.conf" "\
[alt-socks]
type = socks
address = 127.0.0.1
port = 14447
keys = socks-keys.dat")))
  (tunnels-directory i2pd-configuration-tunnels-directory
                     (default "/etc/i2pd/tunnels.conf.d")))

;; I2pd accounts
(define %i2pd-accounts
  ;; User account and groups for I2pd.
  (list (user-group
         (name "i2pd")
         (system? #t))
        (user-account
         (name "i2pd")
         (group "i2pd")
         (system? #t)
         (comment "I2pd daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (i2pd-shepherd-service config)
  "Return a <shepherd-service> running I2pd."
  (let ((i2pd (i2pd-configuration-i2pd config))
        (config-file (i2pd-configuration-config-file config))
        (pid-file (i2pd-configuration-pid-file config))
        (log-file (i2pd-configuration-log-file config))
        (tunnels-config-file (i2pd-configuration-tunnels-config-file config))
        (tunnels-directory (i2pd-configuration-tunnels-directory config)))

    (list (shepherd-service
           (documentation "Run the I2pd daemon.")
           (provision '(i2pd))
           (requirement '(user-processes loopback syslogd))
           (start #~(make-forkexec-constructor
                     (list #$(file-append
                              i2pd
                              "/bin/i2pd")
                           (string-append
                            "--conf="
                            #$config-file)
                           (string-append
                            "--tunconf="
                            #$tunnels-config-file)
                           (string-append
                            "--tunnelsdir="
                            #$tunnels-directory)
                           (string-append
                            "--pidfile="
                            #$pid-file)
                           (string-append
                            "--logfile="
                            #$log-file)
                           "--loglevel=info"
                           "--service")
                     #:user "i2pd"
                     #:group "i2pd"
                     #:pid-file #$pid-file
                     #:log-file #$log-file))
           (stop #~(make-kill-destructor))))))

(define (i2pd-activation config)
  "Set up directories for I2pd and its hidden services, if any."
  (let ((i2pd (i2pd-configuration-i2pd config))
        (config-file (i2pd-configuration-config-file config))
        (tunnels-config-file (i2pd-configuration-tunnels-config-file config))
        (tunnels-directory (i2pd-configuration-tunnels-directory config)))

    (with-imported-modules '((guix build utils))
                           (let ((lib-dir "/var/lib/i2pd")
                                 (run-dir "/var/run/i2pd")
                                 (log-dir "/var/log/i2pd/")
                                 (etc-dir "/etc/i2pd")
                                 (i2pd.conf "/etc/i2pd/i2pd.conf")
                                 (tunnels.conf "/etc/i2pd/tunnels.conf"))

                             #~(begin
                                 (use-modules (guix build utils))

                                 (define %user
                                   (getpw "i2pd"))

                                 ;; Make etc directory to write its CONFIG file.
                                 (mkdir-p #$etc-dir)
                                 (chown #$etc-dir
                                        (passwd:uid %user)
                                        (passwd:gid %user))
                                 (chmod #$etc-dir #o755)

                                 (mkdir-p #$tunnels-directory)
                                 (chown #$tunnels-directory
                                        (passwd:uid %user)
                                        (passwd:gid %user))
                                 (chmod #$tunnels-directory #o755)

                                 ;; Allow I2pd to write its PID file.
                                 (mkdir-p #$run-dir)
                                 (chown #$run-dir
                                        (passwd:uid %user)
                                        (passwd:gid %user))
                                 (chmod #$run-dir #o750)

                                 ;; Allow I2pd to write its Log file.
                                 (mkdir-p #$log-dir)
                                 (chown #$log-dir
                                        (passwd:uid %user)
                                        (passwd:gid %user))
                                 (chmod #$log-dir #o750)

                                 ;; Allow I2pd to access the hidden services' directories.
                                 (mkdir-p #$lib-dir)
                                 (chown #$lib-dir
                                        (passwd:uid %user)
                                        (passwd:gid %user))
                                 (chmod #$lib-dir #o750)
                                 (chmod "/var/lib" #o755)

                                 ;; Copy file to CONFIG file
                                 (copy-file #$config-file
                                            #$i2pd.conf)
                                 (copy-file #$tunnels-config-file
                                            #$tunnels.conf))))))

(define i2pd-service-type
  (service-type (name 'i2pd)
                (extensions (list (service-extension
                                   shepherd-root-service-type
                                   i2pd-shepherd-service)
                                  (service-extension account-service-type
                                                     (const %i2pd-accounts))
                                  (service-extension activation-service-type
                                                     i2pd-activation)))
                (description
                 "Run the @uref{https://i2pd.website, I2pd} anonymous networking daemon")
                (default-value (i2pd-configuration))))
