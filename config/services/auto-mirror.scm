(define-module (config services auto-mirror)
  #:use-module (guix gexp)
  #:use-module (guix git)
  #:use-module (guix git-download)
  #:use-module (guix records)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services version-control)
  #:use-module (gnu system shadow)
  #:use-module (gnu system accounts)
  #:use-module (guix build utils)
  #:use-module (ice-9 ftw)
  #:use-module (git)
  #:export (update-git-mirror-service-type))

(define update-git-mirror!
  (lambda ()
    (let ((base-path "/srv/git"))
    (libgit2-init!)
    (file-system-fold
     (lambda (file stat result)         ; enter?
       (not (string-suffix? ".git" file)))
     (const #t)                          ; leaf
     (lambda (file stat result)          ; down
       (let* ((repo (repository-open file))
              (remote (remote-lookup repo "origin"))
              (proxy "socks://localhost:9050") ; use tor socks5 proxy.
              (options (make-fetch-options #:proxy-url proxy)))
         (remote-fetch remote #:fetch-options options)))
     (const #t)                                ; up
     (const #t)                                ; skip
     (lambda (file stat errno result)          ; error
       (format (current-error-port) "i/o error: ~a: ~a~%"
               file (strerror errno))
       #f)
     #t                                 ; init
     base-path)
    (libgit2-shutdown!))))

(define update-git-mirror-shepherd-type
  (shepherd-service
   (provision '(update-git-mirror))
   (requirement '(user-processes networking git-daemon))
   (modules '((shepherd service timer)))
   (start #~(make-timer-constructor
             (cron-string->calendar-event "0 8 * * *")
             (update-git-mirror)
             #:wait-for-termination? #f))
   (stop #~(make-timer-destructor))
   (documentation "Periodically update git mirror repository in '/srv/git'.")))

(define update-git-mirror-service-type
  (service-type (name 'update-git-mirror)
                (extensions (list (service-extension shepherd-root-service-type
                                                     update-git-mirror-shepherd-type)))
                (description "Periodically update git mirror repository in '/srv/git'.")))
