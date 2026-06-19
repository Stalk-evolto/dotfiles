(define-module (config monads)
  #:use-module (guix monads)
  #:use-module (srfi srfi-9)
  #:export (<maybe>
            make-maybe
            maybe?
            maybe-is?
            maybe-value

            something
            nothing

            %maybe-monad
            maybe-bind

            try-maybe

            <either>
            make-either
            either?
            either-is?
            either-value

            left
            right

            %either-monad
            either-bind
            try-either))

(define-record-type <maybe>
  (make-maybe is? value)
  maybe?
  (is? maybe-is?)
  (value maybe-value))

(define (something value)
  (make-maybe #t value))

(define (nothing)
  (make-maybe #f #f))

(define (maybe-bind maybe proc)
  (if (maybe-is? maybe)
      (proc (maybe-value maybe))
      (nothing)))

(define-monad %maybe-monad
  (bind maybe-bind)
  (return something))

(define (try-maybe maybe)
  (maybe-value maybe))

(define-record-type <either>
  (make-either is? value)
  either?
  (is? either-is?)
  (value either-value))

(define (left value)
  (make-either 'left value))

(define (right value)
  (make-either 'right value))

(define (either-bind either proc)
  (case (either-is? either)
    ((right) (proc (either-value either)))
    ((left) either)))

(define-monad %either-monad
  (bind either-bind)
  (return right))

(define (try-either either)
  (either-value either))
