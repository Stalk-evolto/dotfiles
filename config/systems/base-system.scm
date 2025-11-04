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
;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.

;; Indicate which modules to import to access the variables
;; used in this configuration.
(define-module (config systems base-system)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (nongnu packages linux)
  #:use-module (config services i2pd)
  #:use-module (config packages tor-pluggable)
  #:export (base-system))

(use-package-modules databases
                     golang-web
                     ssh
                     xml)

(use-service-modules admin
                     cups
                     docker
                     monitoring
                     networking
                     spice
                     ssh
                     virtualization
                     xorg
                     base)

(define base-system
  (operating-system
    (kernel linux)
    (firmware (list linux-firmware))
    (locale "en_US.utf8")
    (timezone "Asia/Shanghai")
    (keyboard-layout (keyboard-layout "us"))
    (host-name "stalk-laptop")

    ;; The list of user accounts ('root' is implicit).
    (users (cons* (user-account
                    (name "stalk")
                    (comment "Stalk")
                    (group "users")
                    (home-directory "/home/stalk")
                    (supplementary-groups '("wheel" "netdev" "audio" "video"
                                            "libvirt")))
                  %base-user-accounts))

    ;; Packages installed system-wide.  Users can also install packages
    ;; under their own account: use 'guix search KEYWORD' to search
    ;; for packages and 'guix install PACKAGE' to install a package.
    (packages %base-packages)

    ;; Below is the list of system services.  To search for available
    ;; services, run 'guix system search KEYWORD' in a terminal.
    (services
     (append
      (list
       (service file-database-service-type)
       (service package-database-service-type)

       ;; To configure OpenSSH, pass an 'openssh-configuration'
       ;; record as a second argument to 'service' below.
       (service openssh-service-type
                (openssh-configuration
                  (permit-root-login 'prohibit-password)
                  (password-authentication? #f)
                  (subsystems
                   `(("sftp" ,(file-append openssh "/libexec/sftp-server"))))))
       (service spice-vdagent-service-type)
       (service containerd-service-type)
       (service docker-service-type)
       (service virtlog-service-type
                (virtlog-configuration (max-clients 1000)))
       )

      ;; This is the default list of services
      ;; we are appending to.
      %base-services))

    (bootloader (bootloader-configuration
                  (bootloader grub-efi-bootloader)
                  (targets (list "/boot/efi"))
                  (keyboard-layout keyboard-layout)))
    (swap-devices (list (swap-space
                          (target (file-system-label "swap")))))

    ;; The list of file systems that get "mounted".  The unique
    ;; file system identifiers there ("UUIDs") can be obtained
    ;; by running 'blkid' in a terminal.
    (file-systems (cons* (file-system
                           (mount-point "/home")
                           (device (file-system-label "my-home"))
                           (type "ext4"))
                         (file-system
                           (mount-point "/")
                           (device (file-system-label "my-root"))
                           (type "ext4"))
                         (file-system
                           (mount-point "/boot/efi")
                           (device (uuid "25E4-BEAB"
                                         'fat32))
                           (type "vfat"))
                         %base-file-systems))))
