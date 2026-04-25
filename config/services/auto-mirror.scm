(define-module (config services auto-mirror)
  #:use-module (guix gexp)
  #:use-module (guix git)
  #:use-module (guix git-download)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages guile)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services ssh)
  #:use-module (gnu services version-control)
  #:use-module (gnu system shadow)
  #:use-module (gnu system accounts)
  #:use-module (guix build utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (git)
  #:export (update-git-mirror!
            update-git-mirror-shepherd-type
            update-git-mirror-service-type
            git-ssh-service-type))

(define-record-type* <update-mirror-configuration>
  update-mirror-configuration make-update-mirror-configuration
  update-mirror-configuration?
  (path update-mirror-configuration-path
        (default "/srv/git")))

(define* (update-git-mirror! #:key
                             (base-path "/srv/git")
                             (user "git-daemon")
                             (group "git-daemon"))

  (with-extensions (list guile-git
                         guile-bytestructures)
    #~(lambda ()
        (use-modules (system foreign)
                     (rnrs bytevectors)
                     (system foreign)
                     (ice-9 match)
                     (srfi srfi-1)
                     (git configuration)
                     (git types)
                     (git structs)
                     (git)
                     (git bindings)
                     (git repository)
                     (git remote)
                     (git fetch)
                     (bytestructures guile)
                     (ice-9 ftw))
          (define (find-dirs path)
            (define dir (opendir path))
            (define dirpath-list
              (let f ((entry (readdir dir)) (ls '()))
                (cond
                 ((eof-object? entry) ls)
	         ((not (and
		        (= (stat:uid (stat (string-append path "/" entry)))
                           (passwd:uid (getpw #$user)))
		        (= (stat:gid (stat (string-append path "/" entry)))
                           (group:gid (getgr #$group)))))
	          (f (readdir dir) ls))
                 ((not (or (string=? entry ".")
		           (string=? entry "..")
		           (not (string-suffix? ".git" entry))))
                  (f (readdir dir) (cons (string-append path "/" entry) ls)))
                 (else (f (readdir dir) ls)))))
            (closedir dir)
            dirpath-list)

          (define (update-repository! dir)
            (chown dir (passwd:uid (getpw "root")) (group:gid (getgr "root")))
            (libgit2-init!)
            (let* ((repo (repository-open dir))
                   (remote (remote-lookup repo "origin"))
                   (opt-proxy "http://127.0.0.1:8118") ; use tor HTTPTunnel proxy.
                   (options (make-fetch-options #:proxy-url opt-proxy)))
              (remote-fetch remote #:fetch-options options))
            (libgit2-shutdown!)
            (chown dir (passwd:uid (getpw #$user)) (group:gid (getgr #$group))))

          (let ((dir-list (find-dirs #$base-path)))
            (map update-repository! dir-list)))))

(define (update-git-mirror-shepherd-type config)
  (match-record config <update-mirror-configuration>
                (path)
                (list (shepherd-service
                       (provision '(update-git-mirror))
                       (requirement '(user-processes networking git-daemon))
                       (modules '((shepherd service timer)))
                       (start #~(make-timer-constructor
                                 (cron-string->calendar-event "0 8 * * *")
                                 #$(update-git-mirror! #:base-path path)
                                 #:wait-for-termination? #f))
                       (stop #~(make-timer-destructor))
                       (documentation "Periodically update git mirror repository in '/srv/git'.")
                       (actions (list shepherd-trigger-action))))))

(define update-git-mirror-service-type
  (service-type
   (name 'update-git-mirror-daemon)
   (extensions
    (list (service-extension shepherd-root-service-type
                             update-git-mirror-shepherd-type)))
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
         (shell (file-append git "/bin/git-shell")))))

(define git-ssh-service-type
  (service-type (name 'git-ssh)
                (extensions (list (service-extension openssh-service-type
                                                     identity)
                                  (service-extension account-service-type
                                                     (const %git-ssh-accounts))))
                (description "Git Server ssh allow authorized-keys.")))
