(module connman
  (
   ;;;
   ;;; Services
   ;;;
   connman-discover-services

   ;; Ethernet
   make-ethernet
   ethernet?
   ethernet-method
   ethernet-interface
   ethernet-address
   ethernet-mtu
   ethernet-speed
   ethernet-duplex

   ;; IPv4
   make-ipv4
   ipv4?
   ipv4-method
   ipv4-address
   ipv4-netmask
   ipv4-gateway

   ;; IPv6
   make-ipv6
   ipv6?
   ipv6-method
   ipv6-address
   ipv6-prefix-length
   ipv6-gateway
   ipv6-privacy

   ;; Proxy
   make-proxy
   proxy?
   proxy-method
   proxy-url
   proxy-servers
   proxy-excludes

   ;; Provider
   make-provider
   provider?
   provider-context
   provider-host
   provider-domain
   provider-name
   provider-type

   ;; IPv4 configuration
   make-ipv4-configuration
   ipv4-configuration?
   ipv4-configuration-context
   ipv4-configuration-method
   ipv4-configuration-address
   ipv4-configuration-netmask
   ipv4-configuration-gateway
   ipv4-configuration-set-dhcp!
   ipv4-configuration-set-manual!
   ipv4-configuration-set-off!


   ;; IPv6 configuration
   make-ipv6-configuration
   ipv6-configuration?
   ipv6-configuration-context
   ipv6-configuration-method
   ipv6-configuration-address
   ipv6-configuration-prefix-length
   ipv6-configuration-gateway
   ipv6-configuration-privacy
   ipv6-configuration-set-auto!
   ipv6-configuration-set-off!
   ipv6-configuration-set-manual!

   ;; Proxy configuration
   make-proxy-configuration
   proxy-configuration?
   proxy-configuration-context
   proxy-configuration-method
   proxy-configuration-url
   proxy-configuration-servers
   proxy-configuration-excludes
   proxy-configuration-set-direct!
   proxy-configuration-set-auto!
   proxy-configuration-set-manual!

   ;; Service properties
   service-properties
   service-properties?
   service-properties-context
   service-properties-type
   service-properties-security
   service-properties-state
   service-properties-favorite?
   service-properties-immutable?
   service-properties-auto-connect?
   service-properties-name
   service-properties-login-required?
   service-properties-strength
   service-properties-roaming?
   service-properties-ethernet
   service-properties-ipv4
   service-properties-ipv6
   service-properties-nameservers
   service-properties-domains
   service-properties-proxy
   service-properties-provider
   service-properties-nameservers-configuration
   service-properties-nameservers-configuration-set!
   service-properties-ipv4-configuration
   service-properties-ipv6-configuration
   service-properties-proxy-configuration
   service-properties-domains-configuration
   service-properties-domains-configuration-set!

   make-service-context
   service-connect!
   service-disconnect!
   service-remove!
   service-move-before!
   service-move-after!
   service-reset-counters!

   ;;;
   ;;; Manager
   ;;;
   manager-properties
   manager-properties-state
   manager-properties-offline-mode?
   manager-properties-available-debugs
   manager-properties-enabled-debugs
   manager-properties-session-mode?
   manager-properties-set-offline-mode!
   manager-properties-set-session-mode!

   ;; Technologies
   manager-technologies
   manager-technology-path
   manager-technology-name
   manager-technology-type
   manager-technology-powered?
   manager-technology-connected?
   manager-technology-thetering?

   make-manager-context
   manager-services
   )


(import chicken scheme ports files data-structures extras srfi-1)
(use ssax matchable posix advice)
(use (rename dbus ;; Important: dbus 0.90 is required
             (call dbus-call)
             (make-context dbus-make-context)))

(include "service.scm")
(include "manager.scm")


(define (xml->sxml dbus-call-result)
  (with-input-from-string (car dbus-call-result)
    (lambda ()
      (ssax:xml->sxml (current-input-port) '()))))


;; Get rid of ssax warning messages
(set! ssax:warn (lambda args (void)))


(define-record value-not-set)
(define value-not-set (make-value-not-set))


;; dbus returns #(#<unsupported-type >) to represent empty container
;; values
(define (empty-dbus-value? val)
  (and (vector? val)
       (> (vector-length val) 0)
       (unsupported-type? (vector-ref val 0))))


(define (dbus-value property properties)
  (let ((val (alist-ref property properties equal?)))
    (cond ((empty-dbus-value? val)
           value-not-set)
          ((variant? val)
           (let ((data (variant-data val)))
             (if (empty-dbus-value? data)
                 value-not-set
                 data)))
          (else val))))

) ; end module
