;;  web-server.scm --- Web server config.
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

(define-module (config systems web-server)

  #:use-module (config services shepherd)
  #:use-module (config services)
  #:use-module (config systems minimal)
  #:use-module (fibers channels)
  #:use-module (fibers)
  #:use-module (gnu services cgit)
  #:use-module (gnu services networking)
  #:use-module (gnu services version-control)
  #:use-module (gnu services web)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (guix gexp)
  #:export (%web-server-services
            web-server-system))

(define git-http-service-type
  (service-type (name 'git-http)
                (extensions
                 (list
                  (service-extension
                   tor-service-type
                   (const (list (tor-onion-service-configuration
                                  (name "website")
                                  (mapping '((443 "unix:/var/run/tor/website.sock")))))))
                  (service-extension
                   nginx-service-type
                   (const
                    (list
                     (nginx-server-configuration
                       (listen '("unix:/var/run/tor/website.sock"))

                       ;; (ssl-certificate
                       ;;  "/etc/certs/git/fullchain.pem")
                       ;; (ssl-certificate-key
                       ;;  "/etc/certs/git/privkey.pem")
                       (locations
                        (list
                         (git-http-nginx-location-configuration
                          (git-http-configuration
                            (uri-path "/")))))))))))
                (description "This is Git http server onion service.")
                (default-value '())))

(define %web-server-services
  (list (service git-daemon-service-type
                 (git-daemon-configuration
                   (whitelist '("/srv/git"))))
        (service cgit-service-type
                 ;; (cgit-configuration
                 ;;  (nginx (list (nginx-server-configuration
                 ;;                (listen '("unix:/var/run/tor/website.sock"))))))
                 )

        (service git-http-service-type)

        (service fcgiwrap-service-type)

        (service nginx-service-type
                 (nginx-configuration
                   (shepherd-requirement '(tor))))))

(define web-server-system
  (operating-system
    (inherit %minimal-system)
    (services
     (append
      (list (service shepherd-repl-service-type))
      %web-server-services
      (operating-system-user-services %minimal-system)))))

web-server-system
