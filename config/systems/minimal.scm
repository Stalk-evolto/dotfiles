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
(define-module (config systems minimal)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages less)
  #:use-module (gnu packages man)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu image)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services admin)
  #:use-module (gnu services cups)
  #:use-module (gnu services monitoring)
  #:use-module (gnu services networking)
  #:use-module (gnu services spice)
  #:use-module (gnu services ssh)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services xorg)
  #:use-module (gnu services base)
  #:use-module (gnu system)
  #:use-module (gnu system uuid)
  #:use-module (gnu system image)
  #:use-module (gnu system file-systems)
  #:use-module ((srfi srfi-1) #:hide (partition))
  #:use-module (ice-9 match)
  #:use-module (config packages eclip))

(define %minimal-system
  (operating-system
    (kernel linux-libre)
    (firmware %base-firmware)
    (locale "en_US.utf8")
    (timezone "Asia/Shanghai")
    (keyboard-layout (keyboard-layout "us"))
    (host-name "stalk-virtual")

    ;; The list of user accounts ('root' is implicit).
    (users %base-user-accounts)

    ;; Packages installed system-wide.  Users can also install packages
    ;; under their own account: use 'guix search KEYWORD' to search
    ;; for packages and 'guix install PACKAGE' to install a package.
    (packages (append
               (list less emacs emacs-eclip
                     man-db
                     info-reader
                     kbd
                     sudo
                     guile-readline guile-colorized)
               %base-packages-linux
               %base-packages-networking
               %base-packages-utils))

    ;; Below is the list of system services.  To search for available
    ;; services, run 'guix system search KEYWORD' in a terminal.
    (services
     (append
      (list
       (service openssh-service-type
                (openssh-configuration
                 (permit-root-login 'prohibit-password)
                 (password-authentication? #f)
                 (authorized-keys
                  `(("root"
                     ,(local-file
                       "/home/stalk/Downloads/stalk-phone.pub"))))
                 (subsystems
                  `(("sftp" ,(file-append openssh "/libexec/sftp-server")))))))
      (modify-services %base-services
        (static-networking-service-type
         networks =>
         (list %loopback-static-networking
               %qemu-static-networking)))))

   (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets (list "/boot/efi"))
                 (keyboard-layout keyboard-layout)))

   ;; The list of file systems that get "mounted".  The unique
   ;; file system identifiers there ("UUIDs") can be obtained
   ;; by running 'blkid' in a terminal.
   (file-systems %base-file-systems)))

%minimal-system
