(define-module (config services shepherd)
  #:use-module (shepherd service)
  #:use-module (shepherd service repl)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:export (shepherd-repl-shepherd-service
            shepherd-repl-service-type))

(define (shepherd-repl-shepherd-service socket-file)
  (list (shepherd-service
          (documentation "Run shepherd repl.")
          (provision '(repl))
          (modules '((shepherd service repl)))
          (free-form #~(repl-service #$socket-file)))))

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
