(define-module (tests monads)
  #:use-module (config monads)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix tests)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(define %monads
  (list %maybe-monad %either-monad))

(define %monad-run
  (list try-maybe
        try-either))

(define-syntax-rule (values->list exp)
  (call-with-values (lambda () exp)
    list))

(test-begin "monads")

(test-assert "monad?"
  (and (every monad? %monads)
       (every (compose procedure? monad-bind) %monads)
       (every (compose procedure? monad-return) %monads)))

(test-assert "left identity"
  (every (lambda (monad run)
           (let ((number (random 777)))
             (with-monad monad
               (define (f x)
                 (return (* (1+ number) 2)))

               (= (run (>>= (return number) f))
                  (run (f number))))))
         %monads
         %monad-run))

(test-assert "right identity"
  (every (lambda (monad run)
           (with-monad monad
             (let ((number (return (random 777))))
               (= (run (>>= number return))
                  (run number)))))
         %monads
         %monad-run))

(test-end "monads")
