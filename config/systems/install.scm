;;  install.scm --- guix install operating system with tor.
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

(define-module (config systems install)
  #:use-module (config packages eclip)
  #:use-module (config packages tor)
  #:use-module (gnu bootloader u-boot)
  #:use-module (gnu bootloader)
  #:use-module (gnu installer)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services base)
  #:use-module (gnu services networking)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu system install)
  #:use-module (gnu system)
  #:use-module (gnu)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:export (installation-os-with-tor))

(define installation-os-with-tor
  (operating-system
    (inherit installation-os)

    (packages
     (append
      (list emacs emacs-eclip git)
      (operating-system-packages installation-os)))

    (services
     (append
      (list
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
                           (program (file-append lyrebird "/bin/lyrebird"))))))))
      (modify-services (operating-system-user-services installation-os)
        (guix-service-type config => (guix-configuration
                                       (inherit config)
                                       (discover? #t)
                                       (http-proxy "http://localhost:8118"))))))))

installation-os-with-tor
