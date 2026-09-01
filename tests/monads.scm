(define-module (tests monads)
  #:use-module (config monads)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix tests)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64))

(define %monads
  (list %maybe-monad %either-monad))

(define %monad-run
  (list try-maybe
        try-either))

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

(test-assert "associativity"
  (every (lambda (monad run)
           (with-monad monad
             (define (f x)
               (return (+ 1 x)))
             (define (g x)
               (return (* 2 x)))

             (let ((number (return (random 777))))
               (= (run (>>= (>>= number f) g))
                  (run (>>= number (lambda (x) (>>= (f x) g))))))))
         %monads
         %monad-run))

(test-assert "lift"
  (every (lambda (monad run)
           (let ((f (lift1 1+ monad))
                 (g (apply lift1 1+ (list monad))))
             (with-monad monad
               (let ((number (random 777)))
                 (= (run (>>= (return number) f))
                    (run (>>= (return number) g))
                    (1+ number))))))
         %monads
         %monad-run))

(test-assert ">>= with more than two arguments"
  (every (lambda (monad run)
           (let ((1+ (lift1 1+ monad))
                 (2* (lift1 (cut * 2 <>) monad)))
             (with-monad monad
               (let ((number (random 777)))
                 (= (run (>>= (return number)
                              1+ 1+ 1+
                              2* 2* 2*))
                    (* 8 (+ number 3)))))))
         %monads
         %monad-run))

(test-assert "mbegin"
  (every (lambda (monad run)
           (with-monad monad
             (let* ((been-there? #f)
                    (number (mbegin monad
                              (return 1)
                              (begin
                                (set! been-there? #t)
                                (return 2))
                              (return 3))))
               (and (= (run number) 3)
                    been-there?))))
         %monads
         %monad-run))

(test-assert "mapm"
  (every (lambda (monad run)
           (with-monad monad
             (equal? (run (mapm monad (lift1 1+ monad) (iota 10)))
                     (map 1+ (iota 10)))))
         %monads
         %monad-run))

(test-assert "sequence"
  (every (lambda (monad run)
           (let* ((input (iota 100))
                  (order '()))
             (define (frob i)
               (mlet monad ((foo (return 'foo)))
                 ;; The side effect here is used to keep track of the order in
                 ;; which monadic values are bound.  Perform the side effect
                 ;; within a '>>=' so that it is performed when the return
                 ;; value is actually bound.
                 (set! order (cons i order))
                 (return i)))

             (and (equal? input
                          (run (sequence monad (map frob input))))

                  ;; Make sure this is from left to right.
                  (equal? order (reverse input)))))
         %monads
         %monad-run))

(test-assert "listm"
  (every (lambda (monad run)
           (run (with-monad monad
                  (let ((lst (listm monad
                                    (return 1) (return 2) (return 3))))
                    (mlet monad ((lst lst))
                      (return (equal? '(1 2 3) lst)))))))
         %monads
         %monad-run))

(test-assert "anym"
  (every (lambda (monad run)
           (eq? (run (with-monad monad
                       (anym monad
                             (lift1 (lambda (x)
                                      (and (odd? x) 'odd!))
                                    monad)
                             (append (make-list 1000 0)
                                     (list 1 2)))))
                'odd!))
         %monads
         %monad-run))

(test-end "monads")
