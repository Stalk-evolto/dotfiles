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
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages less)
  #:use-module (gnu packages man)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu image)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services admin)
  #:use-module (gnu services avahi)
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
  #:use-module (config packages eclip)
  #:use-module (config packages tor)
  #:export (%minimal-system))

(define %minimal-system
  (operating-system
    (kernel linux-libre)
    (firmware %base-firmware)
    (locale "en_US.utf8")
    (timezone "Asia/Shanghai")
    (keyboard-layout (keyboard-layout "us"))
    (host-name "stalk-virtual")

    (users %base-user-accounts)

    (packages (append
               (list less emacs emacs-eclip git
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
                        "/home/stalk/.ssh/stalk-phone.pub"))))
                  (subsystems
                   `(("sftp" ,(file-append openssh "/libexec/sftp-server"))))))

       (service tor-service-type
                (tor-configuration
                  (tor tor-latest)
                  (socks-socket-type 'tcp)
                  (config-file (plain-file "torrc"
                                           "\
ClientOnly 1
ClientUseIPv4 1
ClientUseIPv6 1
ClientAutoIPv6ORPort 1
HTTPTunnelPort 8118

UseBridges 1

Bridge obfs4 [2a01:4f9:3070:2c54::122]:8088 7F6051103D00F6E6615C5C8D92C4B648B32331D3 cert=DQ6XOkBQSY424G3SVbOQH5R5aQuWWaCgSI6jv4q7LnI+0h/fJHv4cPX1TMHoY2zD2FUwdQ iat-mode=0
Bridge obfs4 [2a02:ed80:2:1:f816:3eff:feb2:5071]:80 603E097C20A893FA76E997E0AE2079C9F5963818 cert=NPHT6yNMUpZCNOv0ISbingHg3Os3xe/lymPDkLx2cjzu/VptmoFIUsKyRm/aHLS17Kmraw iat-mode=0
Bridge webtunnel [2001:db8:addf:7bc4:155a:a563:a5d5:8b04]:443 F799A0A458365388600F54BD44A99B5887D54911 url=https://aaronstory2026.xyz:2053/vicmackey ver=0.0.4
Bridge webtunnel [2001:db8:fbfa:48b4:5520:53e6:24b4:eca0]:443 93807A85521915D7D2BA17725C08AC39035D1741 url=https://web.localenby.is/HmlgNcBbNgRJw862bJVxZRes ver=0.0.3
"))
                  (transport-plugins
                   (list (tor-transport-plugin
                           (protocol "webtunnel")
                           (program (file-append webtunnel "/bin/client")))
                         (tor-transport-plugin
                           (protocol "obfs4")
                           (program (file-append lyrebird "/bin/lyrebird")))))))

       (service avahi-service-type))

      (modify-services %base-services
        (static-networking-service-type
         networks =>
         (list %loopback-static-networking
               %qemu-static-networking))

        (guix-service-type
         config =>
         (guix-configuration
          (inherit config)
          (discover? #t)
          (http-proxy "http://localhost:8118"))))))

   (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (targets (list "/dev/sda"))
                 (keyboard-layout keyboard-layout)))

   ;; The list of file systems that get "mounted".  The unique
   ;; file system identifiers there ("UUIDs") can be obtained
   ;; by running 'blkid' in a terminal.
   (file-systems (cons (file-system              ;unused
                          (mount-point "/")
                          (device "none")
                          (type "tmpfs"))
                        %base-file-systems))))

%minimal-system
