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
  #:use-module (gnu packages linux)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages xml)
  #:use-module (gnu image)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services admin)
  #:use-module (gnu services cups)
  #:use-module (gnu services docker)
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
  #:export (%base-system
            %build-vm-machine-image))

(define %base-system
  (operating-system
   (kernel linux-libre)
   (firmware %base-firmware)
   (locale "en_US.utf8")
   (timezone "Asia/Shanghai")
   (keyboard-layout (keyboard-layout "us"))
   (host-name "stalk-laptop")

   ;; The list of user accounts ('root' is implicit).
   (users (cons (user-account
                 (name "offload")
                 (group "users")
                 (supplementary-groups '("kvm"))
                 (comment "Account used for offloading"))
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

(define %system-log-message-destination
  ;; Shepherd system log message destination procedure.  Log most messages to
  ;; the console, which goes to the serial output, allowing the host to log
  ;; it.
  #~(lambda (message)
      (cond ((= (system-log-priority debug)
                (system-log-message-priority message))
             '("/var/log/debug"))
            ((member (system-log-message-facility message)
                     (list (system-log-facility authorization)
                           (system-log-facility authorization/private)))
             '("/var/log/secure"))
            (else
             '("/dev/console")))))

(define gc-service-type                           ;TODO: Factorize.
  (shepherd-service-type
   'garbage-collection
   (lambda _
     (shepherd-service
      (provision '(gc))
      (requirement '(user-processes guix-daemon))
      (start #~(make-timer-constructor
                (calendar-event #:minutes '(12))
                (command
                 '("/run/current-system/profile/bin/guix" "gc" "-F2G"))
                #:wait-for-termination? #t))
      (stop #~(make-timer-constructor))
      (actions (list shepherd-trigger-action))))
   #t
   (description "Periodically collect garbage.")))

(define %build-vm-machine-image
  (let* ((type (lookup-image-type-by-name 'mbr-raw))
         (base (os->image %virtual-build-machine-operating-system
                          #:type type)))
    (image (inherit base)
           (format 'compressed-qcow2)
           (partition-table-type 'mbr)
           (volatile-root? #t)
           (shared-store? #f)
           (size (* 20 (expt 2 30)))
           (partitions (match (image-partitions base)
                         ((root)
                          ;; Increase the size of the root partition to match
                          ;; that of the disk image.
                          (let ((root-size (- size (* 50 (expt 2 20)))))
                            (list (partition
                                   (inherit root)
                                   (size root-size))))))))))
