;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Stalk Evolto <stalk-evolto@outlook.com>
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

(define-module (config home services llvm)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services admin)
  #:use-module (gnu packages admin)
  #:use-module (gnu system shadow)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (config packages machine-learning)
  #:export (llama-server-configuration
            llama-server-configuration-module
            llama-server-configuration-port
            llama-server-configuration-memory-size
            llama-server-configuration-extra-options
            llama-server-configuration?

            home-llama-server-service-type))

(define-record-type* <llama-server-configuration> llama-server-configuration
  make-llama-server-configuration
  llama-server-configuration?
  (module llama-server-configuration-module
          (default (car (find-files "/home/stalk/.cache/llama.cpp" "\\.gguf$"))))
  (port llama-server-configuration-port
        (default 5432))
  (threads llama-server-configuration-threads
           (default -1))
  (memory-size llama-server-configuration-memory-size
               (default 4096))
  (extra-options llama-server-configuration-extra-options
                 (default '())))

(define (llama-server-shepherd-service config)
  (let ((module (llama-server-configuration-module config))
        (port (number->string (llama-server-configuration-port config)))
        (threads (number->string (llama-server-configuration-threads config)))
        (memory-size (number->string (llama-server-configuration-memory-size config)))
        (extra-options (llama-server-configuration-extra-options config)))

    (list (shepherd-service
           (documentation "Run the llama-cpp module.")
           (provision '(llama-server))
           (start #~(make-forkexec-constructor
                     (list #$(file-append llama-cpp-latest "/bin/llama-server")
                           "-m" #$module "--port" #$port "--ctx-size" #$memory-size
                           "--threads" #$threads
                           #$@extra-options)))
           (stop #~(make-kill-destructor))))))

(define home-llama-server-service-type
  (service-type
   (name 'llama-server)
   (extensions
    (list (service-extension home-shepherd-service-type
                             llama-server-shepherd-service)))
   (description "Run llama-cpp server.")
   (default-value (llama-server-configuration))))
