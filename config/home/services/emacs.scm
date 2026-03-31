(define-module (config home services emacs)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages emacs)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system emacs)
  #:use-module (guix records)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (home-emacs-configuration
            home-emacs-configuration?
            emacs-configuration-emacs
            home-emacs-service-type))

(define-record-type* <home-emacs-configuration> home-emacs-configuration
  make-emacs-configuration
  home-emacs-configuration?
  (emacs emacs-configuration-emacs
         (default emacs)))

(define (emacs-shepherd-service config)
  (let* ((emacs (emacs-configuration-emacs config))
         (log-file #~(string-append %user-log-dir "/emacs.log")))
    (list (shepherd-service
           (documentation
            "Emacs server.  Use @code{emacsclient} to connect to it.")
           (provision '(emacs-server))
           (modules '((shepherd support)))
           (start #~(make-forkexec-constructor
                     (list #$(file-append emacs "/bin/emacs")
                           "--fg-daemon=server")
                     #:log-file #$log-file))
           (stop #~(make-kill-destructor))))))

(define home-emacs-service-type
  (service-type (name 'emacs)
                (extensions
                 (list (service-extension
                        home-shepherd-service-type
                        emacs-shepherd-service)))
                (default-value (home-emacs-configuration))
                (description "Emacs daemon.")))
