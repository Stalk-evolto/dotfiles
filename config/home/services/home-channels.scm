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

(define-module (config home services home-channels)
  #:use-module (gnu services)
  #:use-module (gnu home services guix)
  #:use-module (guix channels)
  #:export (channels-append-service))

(define channels-append-service
  (simple-service 'nonguix-channels home-channels-service-type
	(list
	 (channel
	  (name 'nonguix)
	  (url "https://gitlab.com/nonguix/nonguix")
	  (branch "master")
	  (commit
	   "0b9e1041aec581d5426adf5fa593e12cc4b75409")
	  (introduction
	   (make-channel-introduction
	    "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
	    (openpgp-fingerprint
	     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))))
