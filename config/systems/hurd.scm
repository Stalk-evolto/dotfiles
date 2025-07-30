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

(define-module (config systems hurd)
  #:use-module (gnu)
  #:use-module (gnu system hurd)
  #:export (childhurd-os))

(use-service-modules virtualization ssh)
(use-package-modules hurd ssh)

(define childhurd-os
  (operating-system
    (inherit %hurd-default-operating-system)
    (kernel %hurd-default-operating-system-kernel)
    (host-name "childhurd")
    (timezone "Asia/Shanghai")
    (locale "en_US.utf8")
    (keyboard-layout (keyboard-layout "us"))
    (file-systems %base-file-systems)
    (bootloader (bootloader-configuration
		 (bootloader grub-minimal-bootloader)
		 (targets '("/dev/vda"))
		 (timeout 0)))

    ;; Add a user account.
    (users (cons* (user-account
                   (name "stalk")
                   (comment "Stalk's hurd")
                   (group "users")
                   (password (crypt "evolto" "$6$abc"))
                   (supplementary-groups '("wheel"))) %base-user-accounts))

    (packages (cons* (operating-system-packages
                      %hurd-default-operating-system)))

    (services
     (modify-services (operating-system-user-services
                       %hurd-vm-operating-system)
       (openssh-service-type config =>
			     (openssh-configuration (permit-root-login #t)
						    (password-authentication? #t)))))))
