(define-module (config services auto-mirror)
  #:use-module ((gnu packages version-control) #:prefix vc:)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages guile)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services ssh)
  #:use-module (gnu services)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (guix build utils)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:export (update-mirror-script
            update-mirror-shepherd-type
            update-mirror-service-type
            git-ssh-service-type))

(define-record-type* <update-mirror-configuration>
  update-mirror-configuration make-update-mirror-configuration
  update-mirror-configuration?
  (path update-mirror-configuration-path
        (default "/srv/git")))

(define (update-mirror-script)
  (define content
    (with-extensions (list guile-git
                           guile-bytestructures)
     (with-imported-modules '((guix monads))
       #~(begin
           (define-module (mirror update)
             #:use-module (guix monads)
             #:use-module (git)
             #:use-module (srfi srfi-1)
             #:use-module (srfi srfi-26)
             #:use-module (srfi srfi-9)
             #:export (with-libgit2
                       %git-monad
                       git-repository-open
                       git-remote
                       git-options
                       git-remote-fetch
                       run-with-git
                       update-repositorys!))

           (define-syntax with-libgit2
             (syntax-rules ()
               ((with-libgit2 exp ...)
                (dynamic-wind (lambda () (libgit2-init!))
                              (lambda () exp ...)
                              (lambda () (libgit2-shutdown!))))))

           (define* (find-dirs path
                               #:key
                               (select? (const #t))
                               (dir (opendir path))
                               (ls '()))
             (let ((entry (readdir dir)))
               (cond
                ((eof-object? entry) (closedir dir) ls)
                ((any (lambda (x) (string=? x entry)) '("." ".."))
                 (find-dirs path #:select? select? #:dir dir #:ls ls))
                ((and (eq? 'directory (stat:type (stat (string-append path "/" entry))))
                      (select? entry))
                 (find-dirs path
                            #:select? select?
                            #:dir dir
                            #:ls (cons (string-append path "/" entry) ls)))
                (else (find-dirs path #:select? select? #:dir dir #:ls ls)))))

           (define* (repository? entry path user group)
             (and
              (= (stat:uid (stat (string-append path "/" entry))) (passwd:uid (getpw user)))
              (= (stat:gid (stat (string-append path "/" entry))) (group:gid (getgr group)))
              (string-suffix? ".git" entry)))

           (define* (find-repository #:optional
                                     (base-path "/srv/git")
                                     (user "git-daemon")
                                     (group "git-daemon"))
             (find-dirs base-path #:select? (cut repository? <> base-path user group)))

           (define (git-return value)
             (lambda ()
               value))

           (define (git-bind mvalue mproc)
             (mproc (mvalue)))

           (define-monad %git-monad
             (bind git-bind)
             (return git-return))

           (define (run-with-git mval)
             (with-libgit2
              (mval)))

           (define (git-repository-open path)
             (lambda ()
               (repository-open path)))

           (define (git-remote repo)
             (lambda ()
               (remote-lookup repo "origin")))

           (define (git-options options)
             (apply make-fetch-options options))

           (define (git-remote-fetch remote options)
             (lambda ()
               (remote-fetch remote #:fetch-options options)))

           (define* (update-repositorys! #:optional
                                         (user "git-daemon")
                                         (group "git-daemon"))
             (let ((paths (find-repository)))
               (setgid (group:gid (getgr group)))
               (setuid (passwd:uid (getpw user)))
               (for-each
                (lambda (path)
                  (run-with-git
                   (mlet* %git-monad ((path -> path)
                                      (repo (git-repository-open path))
                                      (remote (git-remote repo)))
                     (git-remote-fetch remote
                                       (git-options '(#:proxy-url "http://127.0.0.1:8118"))))))
                paths)))
))))
  (scheme-file "update-mirror.scm" content))

(define (update-mirror-script/derivation)
  (with-store store
    (run-with-store store
      (lower-object (update-mirror-script)))))

(define (update-mirror-script/build)
  (mlet* %store-monad
        ((drv -> (update-mirror-script/derivation)))
    (mbegin %store-monad
      (built-derivations (list drv))
      (return (derivation-output-path
               (assoc-ref (derivation-outputs drv) "out"))))))

(define (update-mirror-shepherd-type config)
  (match-record config <update-mirror-configuration>
                (path)
    (list (shepherd-service
            (provision '(update-git-mirror))
            (requirement '(user-processes networking git-daemon))
            (modules '((shepherd service timer)))
            (start
             (with-imported-modules `(((mirror update) => ,(update-mirror-script)))
               #~(begin
                   (use-modules (mirror update))
                   (make-timer-constructor
                    (cron-string->calendar-event "0 8 * * *")
                    (lambda ()
                      (update-repositorys!))
                    #:wait-for-termination? #t))))
            (stop #~(make-timer-destructor))
            (documentation "Periodically update git mirror repository in '/srv/git'.")
            (actions (list shepherd-trigger-action))))))

(define update-mirror-service-type
  (service-type
    (name 'update-mirror-daemon)
    (extensions
     (list (service-extension shepherd-root-service-type
                              update-mirror-shepherd-type)))
    (description "Periodically update git mirror repository in '/srv/git'.")
    (default-value (update-mirror-configuration))))

(define %git-ssh-accounts
  ;; User account and groups for Git Server SSH.
  (list (user-group
          (name "git")
          (system? #f))
        (user-account
          (name "git")
          (group "git")
          (system? #f)
          (comment "Git SSH Server user")
          (home-directory "/srv/git")
          (create-home-directory? #f)
          (shell (file-append vc:git "/bin/git-shell")))))

(define git-ssh-service-type
  (service-type (name 'git-ssh)
                (extensions (list (service-extension openssh-service-type
                                                     identity)
                                  (service-extension account-service-type
                                                     (const %git-ssh-accounts))))
                (description "Git Server ssh allow authorized-keys.")))
