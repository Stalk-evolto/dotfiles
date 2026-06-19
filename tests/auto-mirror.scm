(define-module (tests auto-mirror)
  #:use-module (config services auto-mirror)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix tests)
  #:use-module (srfi srfi-64))

(test-begin "auto-mirror")

(test-assertm "update-mirror-script derivations"
  (lower-object (update-mirror-script)))

(test-assertm "update-mirror-script build"
  (mlet* %store-monad
      ((drv -> (with-store store
                 (run-with-store store
                   (lower-object (update-mirror-script))))))
    (mbegin %store-monad
      (built-derivations (list drv))
      (return (derivation-output-path
               (assoc-ref (derivation-outputs drv) "out"))))))

(test-end "auto-mirror")
