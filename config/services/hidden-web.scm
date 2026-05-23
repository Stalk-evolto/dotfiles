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

(define-module (config services hiddent-web)
  #:use-module (config services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services web)
  #:use-module (gnu services networking)
  #:use-module (gnu services)
  #:use-module (guix records)
  #:use-module (ice-9 textual-ports)
  #:export (onion-service-domains))

(define (onion-service-domains)
  "Return String, the onion service domains."
  (call/cc
   (lambda (service)
    (let ((onion-hostname-file
           (string-append "/var/lib/tor/hidden-services/" service "/hostname")))
      (if (file-exists? onion-hostname-file)
          (call-with-input-file onion-hostname-file get-line))))))
