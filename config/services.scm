;;  services.scm --- Guix service procedure.
;; Copyright (C) 2026  Stalk Evolto <stalk@stalk-laptop>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-module (config services)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (config monads)
  #:use-module (gnu services)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 match)
  #:export (append-service-extensions
            run-with-process
            exec-thunk))

(define (append-service-extensions type lst)
   "Return TYPE, a service type, involve the service extensions
targeting one of the types in LST."
   (service-type
     (inherit type)
     (extensions (append lst
                         (service-type-extensions type)))))

(define (format-supplementary-groups supplementary-groups)
  (list->vector (map (lambda (group) (group:gid (getgr group)))
                     supplementary-groups)))

(define (default-service-directory)
  "Return the default current directory from which a service is started."
  (define (ensure-valid directory)
    (if (and (file-exists? directory)
             (file-is-directory? directory))
        directory
        "/"))

  (if (zero? (getuid))
      "/"
      (ensure-valid (or (getenv "HOME")
                        (and=> (with-exception-handler
                                (lambda (exc) #f)
                                (lambda () (getpw (getuid)))
                                #:unwind? #t)
                               passwd:dir)
                        (getcwd)))))

(define default-environment-variables
  ;; The default list of environment variable name/value pairs that should be
  ;; set when starting a service.
  (make-parameter '()))

(define %current-fork-monitor
  (make-parameter
   (lambda (child-thunk)
     (match (primitive-fork)
       (0
        (dynamic-wind
          (const #t)
          child-thunk
          (lambda ()
            (primitive-exit 127))))
       (pid pid)))))

(define* (run-with-process thunk
                           #:key
                           (monitor (%current-fork-monitor)))
  (parameterize ((%current-fork-monitor monitor))
    (monitor thunk)))

(define (create-session create-session?)
  (with-either-exception-handler
   (lambda (exc)
     (dynamic-wind (const #t)
                   (lambda () exc)
                   (lambda () (primitive-exit 1))))
   (lift0 (lambda ()
            (when create-session?
              ;; Become the leader of a new session and session group.
              ;; Programs such as 'mingetty' expect this.
              (setsid)))
          %either-monad)))

(define (set-resource-limits resource-limits)
  (with-either-exception-handler
   (lambda (exc)
     (dynamic-wind (const #t)
                   (lambda () exc)
                   (lambda () (primitive-exit 1))))
   (lift0 (lambda ()
            (for-each (cut apply setrlimit <>) resource-limits))
          %either-monad)))

(define (change-directory directory)
  (with-either-exception-handler
   (lambda (exc)
     (dynamic-wind (const #t)
                   (lambda () exc)
                   (lambda () (primitive-exit 1))))
   (lift0 (lambda ()
            (when directory
              (chdir directory)))
          %either-monad)))

(define (set-environment-variables environment-variables)
  (with-either-exception-handler
   (lambda (exc)
     (dynamic-wind (const #t)
                   (lambda () exc)
                   (lambda () (primitive-exit 1))))
   (lift0 (lambda ()
            (when environment-variables
              (environ environment-variables)))
          %either-monad)))

(define (set-input-port input-port)
  (with-either-exception-handler
   (lambda (exc)
     (dynamic-wind (const #t)
                   (lambda () exc)
                   (lambda () (primitive-exit 1))))
   (lift0 (lambda ()
            ;; Redirect stdin.
            ;; Make sure file descriptor zero is used, so we don't end up reusing
            ;; it for something unrelated, which can confuse some packages.
            (let ((stdin (if input-port
                             (fileno input-port)
                             (open-fdes "/dev/null" O_RDONLY))))
              (dup2 stdin 0)
              (unless (or input-port (= 0 stdin))
                (close-fdes stdin))))
          %either-monad)))

(define (redirect-log-output log-port log-file extra-ports)
  (with-either-exception-handler
   (lambda (exc)
     (dynamic-wind (const #t)
                   (lambda () exc)
                   (lambda () (primitive-exit 1))))
   (lift0 (lambda ()
            (when (or log-port log-file)
              ;; Redirect stout and stderr to use LOG-FILE.
              (dup2 (if log-file
                        (open-fdes log-file (logior O_CREAT O_WRONLY O_APPEND)
                                   #o640)
                        (fileno log-port))
                    1)
              (dup2 1 2)

              ;; Make EXTRA-PORTS available starting from file descriptor 3.
              ;; This clears their FD_CLOEXEC flag.
              (let loop ((fd    3)
                         (ports extra-ports))
                (match ports
                  (() #t)
                  ((port rest ...)
                   (dup2 (fileno port) fd)
                   (loop (+ 1 fd) rest))))

              ))
          %either-monad)))

(define (set-group group supplementary-groups)
  (with-either-exception-handler
   (lambda (exc)
     (dynamic-wind (const #t)
                   (lambda () exc)
                   (lambda () (primitive-exit 1))))
   (lift0 (lambda ()
            ;; setgid must be done *before* setuid, otherwise the user will
            ;; likely no longer have permissions to setgid.
            (when group
              ;; Clear supplementary groups.
              (setgroups (format-supplementary-groups supplementary-groups))
              (setgid (group:gid (getgr group)))))
          %either-monad)))

(define (set-user user)
  (with-either-exception-handler
   (lambda (exc)
     (dynamic-wind (const #t)
                   (lambda () exc)
                   (lambda () (primitive-exit 1))))
   (lift0 (lambda ()
            (when user
              (setuid (passwd:uid (getpw user)))))
          %either-monad)))

(define (set-file-creation-mask file-creation-mask)
  (with-either-exception-handler
   (lambda (exc)
     (dynamic-wind (const #t)
                   (lambda () exc)
                   (lambda () (primitive-exit 1))))
   (lift0 (lambda ()
            (when file-creation-mask
              (umask file-creation-mask)))
          %either-monad)))

(define (apply-thunk thunk)
  (with-either-exception-handler
   (lambda (exc)
     (dynamic-wind (const #t)
                   (lambda () exc)
                   (lambda () (primitive-exit 1))))
   (lift0 (lambda ()
            (thunk))
          %either-monad)))

(define* (exec-thunk thunk
                     #:key
                     (user #f)
                     (group #f)
                     (supplementary-groups '())
                     (log-file #f)
                     (log-port #f)
                     (input-port #f)
                     (extra-ports '())
                     (directory #f)
                     (file-creation-mask #f)
                     (create-session? #t)
                     (environment-variables #f)
                     (resource-limits '()))
  (lambda ()
    (sequence %either-monad
              (list (create-session create-session?)
                    (set-resource-limits resource-limits)
                    (change-directory directory)
                    (set-environment-variables environment-variables)
                    (set-input-port input-port)
                    (redirect-log-output log-port log-file extra-ports)
                    (set-group group supplementary-groups)
                    (set-user user)
                    (set-file-creation-mask file-creation-mask)
                    (apply-thunk thunk)))))

;; (define %precious-signals
;;   ;; Signals that the shepherd process handles.
;;   (list SIGCHLD SIGINT SIGHUP SIGTERM))

;; (define (handle-unrecoverable-exception thunk exception)
;;   "Handle @var{exception}, raised while attempting to execute @var{command}, by
;; attempting to report it and exiting with a non-zero code."
;;   (define (exception-handle)
;;     (with-either-exception-handler
;;      (lambda (exc) #f)
;;      (lift0
;;       (lambda ()
;;         (format #t (gettext "Failed to run~{ ~s~}: ~a")
;;                 (object->string thunk)
;;                 (if (exception-with-kind-and-args? exception)
;;                     (string-trim-right
;;                      (call-with-output-string
;;                        (lambda (port)
;;                          (print-exception port #f
;;                                           (exception-kind exception)
;;                                           (exception-args exception)))))
;;                     (object->string exception)))
;;         (newline))
;;       %either-monad)))
;;   (dynamic-wind
;;     (const #t)
;;     exception-handle
;;     (lambda () (primitive-exit 127))))

;; (define-syntax-rule (with-exit-on-failure thunk exp ...)
;;   "Evaluate @var{exp}.  Exit with a non-zero code if an exception is raised."
;;   (with-either-exception-handler
;;    (lambda (exception)
;;      (handle-unrecoverable-exception thunk exception))
;;    (lambda () exp ...)))

;; (define (set-port-encoding port enc)
;;   (with-either-exception-handler
;;    (lambda (exc)
;;      (lambda () exc))
;;    (lift0 (lambda ()
;;             (when enc
;;            (set-port-encoding! port enc)))
;;           %either-monad)))

;; (define (set-port-conversion-strategy port sym)
;;   (with-either-exception-handler
;;    (lambda (exc)
;;      (lambda () exc))
;;    (lift0 (lambda ()
;;             (set-port-conversion-strategy! port sym))
;;           %either-monad)))

;; (define* (fork+exec-thunk thunk
;;                           #:key
;;                           (user #f)
;;                           (group #f)
;;                           (supplementary-groups '())
;;                           (log-file #f)
;;                           (input-port #f)
;;                           (log-encoding "UTF-8")
;;                           (extra-ports '())
;;                           (directory (default-service-directory))
;;                           (file-creation-mask #f)
;;                           (create-session? #t)
;;                           (environment-variables
;;                            (default-environment-variables))
;;                           (listen-pid-variable? #f)
;;                           (resource-limits '()))

;;   (define (child-thunk log-input log-output)
;;     (lambda ()
;;       ;; Exit the child process with non-zero when something throws, such as the
;;       ;; 'chdir' or 'execl' calls made by 'exec-command'.
;;       (with-exit-on-failure thunk
;;         ;; First restore the default handlers.
;;         (for-each (cut sigaction <> SIG_DFL) %precious-signals)

;;         ;; Unblock any signals that have been blocked by the parent process.
;;         (unblock-signals %precious-signals)

;;         (close-port log-input)
;;         ((exec-thunk thunk
;;                      #:user user
;;                      #:group group
;;                      #:supplementary-groups supplementary-groups
;;                      #:log-port log-output
;;                      #:input-port input-port
;;                      #:extra-ports extra-ports
;;                      #:directory directory
;;                      #:file-creation-mask file-creation-mask
;;                      #:create-session? create-session?
;;                      #:environment-variables
;;                      (if listen-pid-variable?
;;                          (cons (string-append "LISTEN_PID="
;;                                               (number->string (getpid)))
;;                                environment-variables)
;;                          environment-variables)
;;                      #:resource-limits resource-limits)))))

;;     ;; Child processes inherit signal handlers until they exec.  If one of
;;   ;; %PRECIOUS-SIGNALS is received by the child before it execs, the installed
;;   ;; handler, which stops shepherd, is called.  To avoid this, block signals
;;   ;; so that the child process never executes those handlers.
;;   (with-blocked-signals %precious-signals
;;     (match (pipe O_NONBLOCK)
;;       ((log-input . log-output)
;;        (let* ((pid (run-with-process (child-thunk log-input log-output)))
;;               (log-input (non-blocking-port log-input)))
;;          (close-port log-output)
;;          (sequence %either-monad
;;                    (list (set-port-encoding log-input log-encoding)
;;                          (set-port-conversion-strategy log-input 'substitute)))
;;          pid)))))
