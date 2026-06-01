;;  hidden-web.scm --- Hiddent web server service.
;; Copyright (C) 2026  System administrator <root@localhost>

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

(define-module (config services hidden-web)
  #:use-module (config services)
  #:use-module (fibers channels)
  #:use-module (fibers)
  #:use-module (gnu services networking)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services web)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:export (onion-service-domain
            spawn-onion-service-domain
            onion-server-record))

(define-record-type* <onion-nginx-server-configuration> onion-nginx-server-configuration
  make-onion-nginx-server-configuration
  onion-nginx-server-configuration?
  (hidden-services onion-nginx-server-configuration-hidden-services
                   (default '()))       ;list of <tor-onion-service-configuration>
  (server-blocks onion-nginx-server-configuration-server-blocks
                 (default '()))          ;list of <nginx-server-configuration>
  )

(define (onion-service-domain channel service)
  "Return String, the onion service domains."
  (let loop ((onion-hostname-file
              (string-append "/var/lib/tor/hidden-services/" service "/hostname")))
    (if (file-exists? onion-hostname-file)
        (put-message channel
                     (call-with-input-file onion-hostname-file get-line))
        (loop onion-hostname-file))))

(define (spawn-onion-service-domain service)
  (let ((channel (make-channel)))
    (spawn-fiber
     (lambda ()
       (onion-service-domain channel service)))
    channel))

(define (set-nginx-server-configuration-server-name config var)
  "Return a copy of NGINX-SERVER-CONFIGURATION where SERVER-NAME has the SERVER-NAMES.
SERVER-NAMES must be a string in list."
  (nginx-server-configuration
   (inherit config)
   (server-name var)))

(define (set-nginx-server-configuration-listen config var)
  "Return a copy of NGINX-SERVER-CONFIGURATION where LISTEN has the LISTENS.
LISTENS must be a string in list."
  (nginx-server-configuration
   (inherit config)
   (listen var)))

(define (operating-system-nginx-server-blocks-merge os var)
  (operating-system
    (inherit os)
    (services
     (modify-services (operating-system-user-services os)
       (nginx-service-type
        config => (nginx-configuration
                    (inherit config)
                    (server-blocks (append
                                    (nginx-configuration-server-blocks config)
                                    var))))))))

(define (wrapper-nginx-server-blocks config)
  (let* ((hidden-services (onion-nginx-server-configuration-hidden-services config))
         (service-names (map tor-onion-service-configuration-name hidden-services))
         (listens (delete-duplicates
                   (map (lambda (mapping) (cdr mapping))
                        (map tor-onion-service-configuration-mapping hidden-services))))
         (server-blocks (onion-nginx-server-configuration-server-blocks config)))
    (run-fibers
     (lambda ()
       (map
        (lambda (server-block listen service-name)
          (set-nginx-server-configuration-server-name
           (set-nginx-server-configuration-listen server-block listen)
           (get-message (spawn-onion-service-domain service-name))))
        server-blocks listens service-names)))))

(define (onion-nginx-extension-merge a b)
  (onion-nginx-server-configuration
   (hidden-services
    (append
     (onion-nginx-server-configuration-hidden-services a)
     (onion-nginx-server-configuration-hidden-services b)))
   (server-blocks
    (append
     (onion-nginx-server-configuration-server-blocks a)
     (onion-nginx-server-configuration-hidden-services b)))))

(define onion-nginx-service-type
  (service-type (name 'onion-nginx)
                (extensions
                 (list
                  (service-extension
                   nginx-service-type
                   (const (lambda (config)
                            (onion-nginx-server-configuration-server-blocks config))))
                  (service-extension
                   tor-service-type
                   (const (lambda (config)
                            (onion-nginx-server-configuration-hidden-services config))))))
                (compose (lambda (args)
                           (fold onion-nginx-extension-merge
                                 (onion-nginx-server-configuration)
                                 args)))
                (extend
                 (lambda (config extension)
                   (onion-nginx-server-configuration
                    (inherit config)
                    (hidden-services
                     (append
                      (onion-nginx-server-configuration-hidden-services config)
                      (onion-nginx-server-configuration-hidden-services extension)))
                    (server-blocks
                     (append
                      (onion-nginx-server-configuration-server-blocks config)
                      (onion-nginx-server-configuration-server-blocks extension))))))
                (default-value (onion-nginx-server-configuration))
                (description "Run the nginx with tor domain Web server.")))
