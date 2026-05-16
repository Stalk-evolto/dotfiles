;;  services.scm --- Guix service procedure.
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

(define-module (config services)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 textual-ports)
  #:export (append-service-extensions
            onion-service-domains))

(define (append-service-extensions type lst)
   "Return TYPE, a service type, involve the service extensions
targeting one of the types in LST."
   (service-type
     (inherit type)
     (extensions (append lst
                         (service-type-extensions type)))))

;; (define (onion-service-domains service)
;;   "Return String, the onion service domains."
;;   #~(begin
;;       (use-modules (ice-9 textual-ports))
;;     (call-with-input-file
;;        (string-append "/var/lib/tor/hidden-services/" #$service "/hostname")
;;       get-line)))
