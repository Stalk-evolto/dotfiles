(define-module (config services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (guix gexp)
  #:export (shepherd-repl-shepherd-service
            shepherd-repl-service-type))

(define %runstatedir "/var/run")

(define %user-runtime-dir
  ;; Default runtime directory if shepherd is run as a normal user.
  (string-append (or (getenv "XDG_RUNTIME_DIR")
                     (format #f "/run/user/~s" (getuid)))))
(define %system-socket-dir
  (string-append %runstatedir "/shepherd"))

(define default-socket-dir
  (if (zero? (getuid))
      %system-socket-dir
      (string-append %user-runtime-dir "/shepherd")))

(define default-repl-socket-file
  ;; Default socket file for the REPL.
  (make-parameter (string-append default-socket-dir "/repl")))

(define (shepherd-repl-shepherd-service socket-file)
  (list (shepherd-service
          (documentation "Run shepherd repl.")
          (provision '(repl))
          (modules '((shepherd service)
                     (shepherd service repl)))
          (free-form (with-extensions (list shepherd)
                       #~(begin
                           (use-modules (shepherd service)
                                        (shepherd service repl))
                           (repl-service #$socket-file)))))))

(define shepherd-repl-service-type
  (service-type
   (name 'repl)
   (extensions
    (list (service-extension
           shepherd-root-service-type
           shepherd-repl-shepherd-service)))
   (description
    "Run the @uref{https://doc.guix.gnu.org/shepherd/latest/en/html_node/REPL-Service.html, Shepherd REPL}.")
   (default-value (default-repl-socket-file))))
