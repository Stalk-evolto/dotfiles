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
(define-module (config systems system)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (nongnu packages linux)
  #:use-module (gnu packages golang-web)
  #:use-module (config systems hurd)
  #:use-module (config services i2pd))

(use-service-modules base
                     cups
                     desktop
                     networking
                     ssh
                     xorg
                     spice
                     docker
                     virtualization)

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
                                          "libvirt"))) %base-user-accounts))

  ;; Packages installed system-wide.  Users can also install packages
  ;; under their own account: use 'guix search KEYWORD' to search
  ;; for packages and 'guix install PACKAGE' to install a package.
  (packages (append (map specification->package
                         '("font-wqy-zenhei" "font-gnu-unifont" "ibus"
                           "ibus-libpinyin" "dconf" "git")) %base-packages))

  ;; Below is the list of system services.  To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
   (append (list (service gnome-desktop-service-type)

                 ;; To configure OpenSSH, pass an 'openssh-configuration'
                 ;; record as a second argument to 'service' below.
                 (service openssh-service-type)
                 (service spice-vdagent-service-type)
                 (service containerd-service-type)
                 (service docker-service-type)
                 (service i2pd-service-type)
                 (service tor-service-type
                          (tor-configuration (socks-socket-type 'tcp)
                                             (config-file (local-file
                                                           "/etc/tor/torrc"))
                                             (transport-plugins (list (tor-transport-plugin
                                                                       (protocol
                                                                        "meek_lite,obfs2,obfs3,obfs4,scramblesuit,webtunnel")
                                                                       (program
                                                                        (file-append
                                                                         lyrebird
                                                                         "/bin/lyrebird")))))))
                 (service libvirt-service-type
                          (libvirt-configuration (unix-sock-group "libvirt")
                                                 (tls-port "16555")))
                 (service qemu-binfmt-service-type
                          (qemu-binfmt-configuration
                           (platforms
                            (lookup-qemu-platforms "arm" "aarch64"))))

                 (service virtlog-service-type
                          (virtlog-configuration (max-clients 1000)))
                 (service hurd-vm-service-type
                          (hurd-vm-configuration (os childhurd-os)
                                                 (disk-size (* 5000
                                                               (expt 2 20)))
                                                 (memory-size 1024)))
                 (set-xorg-configuration
                  (xorg-configuration (keyboard-layout keyboard-layout))))

           ;; This is the default list of services
           ;; we are appending to.
           (modify-services %desktop-services
             (guix-service-type config =>
                                (guix-configuration (inherit config)
                                                    ;; (http-proxy "http://192.168.1.10:10809")
                                                    )))))

  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))
  (swap-devices (list (swap-space
                        (target (uuid "0b2df261-f1cb-4577-820d-a309e08913e0")))))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
  (file-systems (cons* (file-system
                         (mount-point "/home")
                         (device (uuid "40b7f246-48d0-4a13-b5d6-006df20d2fce"
                                       'ext4))
                         (type "ext4"))
                       (file-system
                         (mount-point "/")
                         (device (uuid "3aef86c9-3c3b-4fe0-9a43-e537abc6221a"
                                       'ext4))
                         (type "ext4"))
                       (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "25E4-BEAB"
                                       'fat32))
                         (type "vfat")) %base-file-systems)))
