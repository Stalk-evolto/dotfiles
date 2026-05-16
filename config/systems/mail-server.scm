;;  mail-server.scm --- Mail server operating system define.
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
(define-module (config systems mail-server)
  #:use-module (config packages mail)
  #:use-module (config packages tor)
  #:use-module (config services shepherd)
  #:use-module (config services)
  #:use-module (config systems minimal)
  #:use-module (gnu build accounts)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages tls)
  #:use-module (gnu services configuration)
  #:use-module (gnu services mail)
  #:use-module (gnu services networking)
  #:use-module (gnu services)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (gnu system)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (ice-9 textual-ports)
  #:export (%mail-server-services
            mail-server-system))

(define %mail-virtual-accounts
  (list (user-group (name "vmail") (system? #t))
        (user-account
          (name "vmail")
          (group "vmail")
          (home-directory "/home/vmail"))))

(define %mail-cert-accounts
  (list (user-group (name "mail") (system? #t))
        (user-account
          (name "mail")
          (group "mail")
          (supplementary-groups '("dovecot" "dovenull" "exim"))
          (system? #t)
          (comment "Mail service user")
          (home-directory "/var/empty")
          (create-home-directory? #f)
          (shell (file-append shadow "/sbin/nologin")))))

(define (onion-service-domains service)
  "Return String, the onion service domains."
  (call-with-input-file
      (string-append "/var/lib/tor/hidden-services/" service "/hostname")
    get-line))

(define (%mail-activation config)
  (let ((public-key "/etc/cert/mail/fullchain.pem")
        (private-key "/etc/cert/mail/privkey.pem"))
    (with-imported-modules (source-module-closure '((gnu build activation)))
      #~(begin
          (use-modules (guix build utils) (gnu build activation))
          (define (build-subject parameters)
            (string-concatenate
             (map (lambda (pair)
                    (let ((k (car pair)) (v (cdr pair)))
                      (define (escape-char str chr)
                        (string-join (string-split str chr) (string #\\ chr)))
                      (string-append "/" k "="
                                     (escape-char (escape-char v #\=) #\/))))
                  (filter (lambda (pair) (cdr pair)) parameters))))
          (define* (create-self-signed-certificate-if-absent
                    #:key private-key public-key (owner (getpwnam "mail"))
                    (common-name (gethostname))
                    (organization-name "Guix")
                    (organization-unit-name "Default Self-Signed Certificate")
                    (subject-parameters `(("CN" . ,common-name)
                                          ("O" . ,organization-name)
                                          ("OU" . ,organization-unit-name)))
                    (subject (build-subject subject-parameters)))
            ;; Note that by default, OpenSSL outputs keys in PEM format.  This
            ;; is what we want.
            (unless (file-exists? private-key)
              (cond
               ((zero? (system* (string-append #$openssl "/bin/openssl")
                                "genrsa" "-out" private-key "2048"))
                (chown private-key (passwd:uid owner) (passwd:gid owner))
                (chmod private-key #o440))
               (else
                (format (current-error-port)
                        "Failed to create private key at ~a.\n" private-key))))
            (unless (file-exists? public-key)
              (cond
               ((zero? (system* (string-append #$openssl "/bin/openssl")
                                "req" "-new" "-x509" "-key" private-key
                                "-out" public-key "-days" "3650"
                                "-batch" "-subj" subject))
                (chown public-key (passwd:uid owner) (passwd:gid owner))
                (chmod public-key #o444))
               (else
                (format (current-error-port)
                        "Failed to create public key at ~a.\n" public-key)))))
          (let ((user (getpwnam "mail")))
            (mkdir-p/perms "/etc/cert" user #o755)
            (mkdir-p/perms "/etc/cert/mail" user #o755)
            (create-self-signed-certificate-if-absent
             #:private-key #$private-key
             #:public-key #$public-key
             #:owner user
             #:common-name (format #f "Onion mail service on ~a" (gethostname))))))))

(define mail-server-service-type
  (service-type (name 'mail-server)
                (extensions
                 (list (service-extension account-service-type
                                          (const (append %mail-cert-accounts
                                                          %mail-virtual-accounts)))
                       (service-extension activation-service-type
                                          %mail-activation)
                       (service-extension tor-service-type
                                          (const (list
                                                  (tor-onion-service-configuration
                                                   (name "mail")
                                                   (mapping '((143 "127.0.0.1:143")
                                                              (993 "127.0.0.1:993")
                                                              (110 "127.0.0.1:110")
                                                              (995 "127.0.0.1:995")
                                                              (25 "127.0.0.1:25")))))))))
                (description "This is Mail server onion service.")
                (default-value '())))

(define %mail-server-services
  (list (service mail-server-service-type)
        (service (append-service-extensions
                  (remove-service-extensions dovecot-service-type
                                             (list account-service-type))
                  (list
                   (service-extension
                    account-service-type
                    (const (list (user-group (name "dovecot") (system? #t))
                                 (user-account
                                   (name "dovecot")
                                   (group "dovecot")
                                   (supplementary-groups '("mail"))
                                   (system? #t)
                                   (comment "Dovecot daemon user")
                                   (home-directory "/var/empty")
                                   (shell (file-append shadow "/sbin/nologin")))

                                 (user-group (name "dovenull") (system? #t))
                                 (user-account
                                   (name "dovenull")
                                   (group "dovenull")
                                   (system? #t)
                                   (comment "Dovecot daemon login user")
                                   (home-directory "/var/empty")
                                   (shell (file-append shadow "/sbin/nologin"))))))))
                 (dovecot-configuration
                   (mail-location "maildir:/var/vmail/%d/%n")
                   (auth-verbose? #t)
                   (ssl-cert "</etc/cert/mail/fullchain.pem")
                   (ssl-key "</etc/cert/mail/privkey.pem")
                   (auth-mechanisms '("plain" "login"))
                   (log-path "/var/log/dovecot.log")
                   (info-log-path "/var/log/dovecot-info.log")
                   (protocols
                    (list (protocol-configuration
                            (name "imap"))
                          (protocol-configuration
                            (name "lmtp"))))
                   (services
                    (list
                     (service-configuration
                       (kind "imap-login")
                       (client-limit 0)
                       (process-limit 0)
                       (listeners
                        (list
                         (inet-listener-configuration (protocol "imap") (port 143) (ssl? #f))
                         (inet-listener-configuration (protocol "imaps") (port 993) (ssl? #t)))))
                     (service-configuration
                       (kind "pop3-login")
                       (listeners
                        (list
                         (inet-listener-configuration (protocol "pop3") (port 110) (ssl? #f))
                         (inet-listener-configuration (protocol "pop3s") (port 995) (ssl? #t)))))
                     (service-configuration
                       (kind "lmtp")
                       (client-limit 1)
                       (process-limit 0)
                       (listeners
                        (list (unix-listener-configuration (path "lmtp")
                                                           (mode "0666")
                                                           (user "exim")
                                                           (group "exim")))))
                     (service-configuration
                       (kind "imap")
                       (client-limit 1)
                       (process-limit 1024))
                     (service-configuration
                       (kind "pop3")
                       (client-limit 1)
                       (process-limit 1024))
                     (service-configuration
                       (kind "auth")
                       (service-count 0)
                       (client-limit 0)
                       (process-limit 1)
                       (listeners
                        (list (unix-listener-configuration (path "auth-userdb")
                                                           (user "dovecot")
                                                           (group "dovecot"))
                              (unix-listener-configuration (path "auth-client")
                                                           (mode "0660")
                                                           (user "exim")
                                                           (group "exim")))))
                     (service-configuration
                       (kind "auth-worker")
                       (client-limit 1)
                       (process-limit 0))
                     (service-configuration
                       (kind "dict")
                       (client-limit 1)
                       (process-limit 0)
                       (listeners (list (unix-listener-configuration (path "dict")))))))
                   (passdbs (list
                             (passdb-configuration
                               (driver "sql")
                               (args
                                '("/etc/dovecot/dovecot-sql.conf.ext")))))
                   (userdbs (list
                             (userdb-configuration
                               (driver "sql")
                               (args
                                '("/etc/dovecot/dovecot-sql.conf.ext")))))))

        (service (append-service-extensions
                  (remove-service-extensions exim-service-type
                                             (list account-service-type))
                  (list
                   (service-extension
                    account-service-type
                    (const (list (user-group (name "exim") (system? #t))
                                 (user-account
                                   (name "exim")
                                   (group "exim")
                                   (supplementary-groups '("mail"))
                                   (system? #t)
                                   (comment "Exim Daemon")
                                   (home-directory "/var/empty")
                                   (shell (file-append shadow "/sbin/nologin"))))))))
                 (exim-configuration
                   (package exim-latest)
                   (config-file (local-file "aux-files/exim.conf"))))

        (service mail-aliases-service-type
                 '(("postmaster" "vmail")
                   ("abuse" "vmail")
                   ("webmaster" "vmail")))))

(define mail-server-system
  (operating-system
    (inherit %minimal-system)
    (services
     (append
      (list (service shepherd-repl-service-type))
      %mail-server-services
      (operating-system-user-services %minimal-system)))))

mail-server-system
