(define-module (config scripts git)
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

(define* (update-repositorys!)
  (let ((paths (find-repository)))
    (for-each
     (lambda (path)
       (run-with-git
        (mlet* %git-monad ((path -> path)
                           (repo (git-repository-open path))
                           (remote (git-remote repo)))
          (git-remote-fetch remote
                            (git-options '(#:proxy-url "http://127.0.0.1:8118"))))))
     paths)))
