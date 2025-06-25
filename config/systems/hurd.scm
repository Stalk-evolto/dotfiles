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
