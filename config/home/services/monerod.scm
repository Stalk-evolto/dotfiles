(define-module (config home services monerod)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages finance)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix records)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (rnrs enums)
  #:use-module (ice-9 match)
  #:export (home-monerod-configuration
            home-monerod-configuration?
            monerod-configuration-monerod
            monerod-configuration-extra-options

            tx-proxy-configuration
            tx-proxy-configuration?
            tx-proxy-configuration-type
            tx-proxy-configuration-address
            tx-proxy-configuration-port
            tx-proxy-configuration-max-connect

            anonymous-inbound-configuration
            anonymous-inbound-configuration?
            anonymous-inbound-configuration-address
            anonymous-inbound-configuration-host-local
            anonymous-inbound-configuration-max-connect

            monerod-shepherd-service
            home-monerod-service-type))

(define-record-type* <home-monerod-configuration> home-monerod-configuration
  make-monerod-configuration
  home-monerod-configuration?
  (monerod monerod-configuration-monerod
           (default monero))
  (proxy monerod-configuration-proxy
         (default #f))
  (p2p-bind-ip monerod-configuration-p2p-bind-ip
               (default #f))
  (no-igd? monerod-configuration-no-igd?
           (default #f))
  (tx-proxy monerod-configuration-tx-proxy
            (default '()))
  (anonymous-inbound monerod-configuration-anonymous-inbound
                     (default '()))
  (extra-options monerod-configuration-extra-options
                 (default '("--rpc-restricted-bind-port" "18089"
                            "--prune-blockchain"
                            "--sync-pruned-blocks"))))

(define-configuration/no-serialization tx-proxy-configuration
  (type
   (string "tor")
   "Onion Service type. Value is tor or i2p.")

  (address
   (string "127.0.0.1")
   "Onion service address.")

  (port
   (number 9050)
   "Onion service port.")

  (max-connect
   (number 10)
   "Max of outgoing connections."))

(define-configuration/no-serialization anonymous-inbound-configuration

  (address
   string
   "Anonymous inbound address.")

  (host-local
   (string "127.0.0.1:18081")
   "Monerod local service ip address and port.")

  (max-connect
   (number 25)
   "Max of inbound connections."))

(define (monerod-shepherd-service config)
  (match-record
   config <home-monerod-configuration>
   (monerod proxy p2p-bind-ip no-igd? tx-proxy anonymous-inbound extra-options)
   (let* ((log-file #~(string-append %user-log-dir "/menerod.log")))
     (define tx-proxys
       (if (null? tx-proxy) '()
           (map (match-lambda
                  ((type address port max-connect)
                   `("--tx-proxy"
                     ,(format #f "~a,~a:~a,~a"
                             type address port max-connect))))
                (map (match-lambda
                       (($ <tx-proxy-configuration> type address port max-connect)
                        (list type
                              address
                              port
                              max-connect)))
                     tx-proxy))))
     (define anonymous-inbounds
       (if (null? anonymous-inbound) '()
           (map (match-lambda
                  ((address host-local max-connect)
                   `("--anonymous-inbound"
                     ,(format #f "~a,~a,~a"
                             address host-local max-connect))))
                (map (match-lambda
                       (($ <anonymous-inbound-configuration> address host-local max-connect)
                        (list address
                              host-local
                              max-connect)))
                     anonymous-inbound))))
     (list (shepherd-service
            (documentation
             "Monero daemon. Use @code{monero-wallet-cli} connect to it.")
            (provision '(monero-daemon))
            (modules '((shepherd support)))
            (start #~(make-forkexec-constructor
                      (list #$(file-append monerod "/bin/monerod")
                            "--non-interactive"
                            #$@(when proxy
                                 #~("--proxy" #$proxy))
                            #$@(when p2p-bind-ip
                                 #~("--p2p-bind-ip" #$p2p-bind-ip))
                            #$@(when no-igd?
                                 '("--no-igd"))
                            #$@(apply append tx-proxys)
                            #$@(apply append anonymous-inbounds)
                            #$@extra-options)
                      #:log-file #$log-file))
            (stop #~(make-kill-destructor)))))))

(define home-monerod-service-type
  (service-type (name 'monerod)
                (extensions
                 (list (service-extension
                        home-shepherd-service-type
                        monerod-shepherd-service)))
                (default-value (home-monerod-configuration))
                (description "Monero daemon.")))
