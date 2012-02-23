(define (connman-discover-services)
  ;; FIXME: handle errors (depends on dbus egg raising errors)
  (let* ((c (dbus-make-context bus: system-bus
                               service: 'net.connman
                               interface: 'org.freedesktop.DBus.Introspectable
                               path: '/net/connman/service))
         (result (xml->sxml (dbus-call c "Introspect")))
         (nodes (cdar (cdr result))))
    (map (lambda (node)
           (match node
             ((node (@ (_name name))) name)))
         nodes)))


(define-record ethernet method interface address mtu speed duplex)
(define-record ipv4 method address netmask gateway)
(define-record ipv6 method address prefix-length gateway privacy)
(define-record proxy method url servers excludes)
(define-record ipv4-configuration context method address netmask gateway)
(define-record ipv6-configuration context method address prefix-length gateway privacy)
(define-record proxy-configuration context method url servers excludes)
(define-record provider context host domain name type)

(define-record service-properties
  context
  type
  security
  state
  favorite?
  immutable?
  auto-connect?
  name
  login-required?
  strength
  roaming?
  ethernet
  ipv4
  ipv6
  nameservers
  domains
  proxy
  provider
  nameservers-configuration
  ipv4-configuration
  ipv6-configuration
  proxy-configuration
  domains-configuration)


(define (make-service-context service)
  (dbus-make-context bus: system-bus
                     service: 'net.connman
                     interface: 'net.connman.Service
                     path: (make-pathname "/net/connman/service"
                                          service)))


(define (service-properties service-context)
  (let (;; FIXME: handle errors (depends on dbus egg raising errors)
        (properties (dbus-call service-context "GetProperties")))
    (if (or (not properties) (null? properties))
        #f
        (let* ((props (vector->list (car properties))))
          (make-service-properties
           service-context
           (dbus-value "Type" props)
           (dbus-value "Security" props)
           (dbus-value "State" props)
           (dbus-value "Favorite" props)
           (dbus-value "Immutable" props)
           (dbus-value "AutoConnect" props)
           (dbus-value "Name" props)
           (dbus-value "LoginRequired" props)
           (dbus-value "Strength" props)
           (dbus-value "Roaming" props)
           (or (and-let* ((ethernet (dbus-value "Ethernet" props))
                          (_ (and (not (empty-dbus-value? ethernet))
                                  (not (value-not-set? ethernet))))
                          (ethernet (vector->list ethernet)))
                 (make-ethernet
                  (dbus-value "Method" ethernet)
                  (dbus-value "Interface" ethernet)
                  (dbus-value "Address" ethernet)
                  (dbus-value "MTU" ethernet)
                  (dbus-value "Speed" ethernet)
                  (dbus-value "Duplex" ethernet)))
               (make-ethernet
                value-not-set value-not-set value-not-set value-not-set value-not-set value-not-set))
           (or (and-let* ((ipv4 (dbus-value "IPv4" props))
                          (_ (and (not (empty-dbus-value? ipv4))
                                  (not (value-not-set? ipv4))))
                          (ipv4 (vector->list ipv4)))
                 (make-ipv4
                  (dbus-value "Method" ipv4)
                  (dbus-value "Address" ipv4)
                  (dbus-value "Netmask" ipv4)
                  (dbus-value "Gateway" ipv4)))
               (make-ipv4 value-not-set value-not-set value-not-set value-not-set))
           (or (and-let* ((ipv6 (dbus-value "IPv6" props))
                          (_ (and (not (empty-dbus-value? ipv6))
                                  (not (value-not-set? ipv6))))
                          (ipv6 (vector->list ipv6)))
                 (make-ipv6
                  (dbus-value "Method" ipv6)
                  (dbus-value "Address" ipv6)
                  (dbus-value "PrefixLength" ipv6)
                  (dbus-value "Gateway" ipv6)
                  (dbus-value "Privacy" ipv6)))
               (make-ipv6 value-not-set value-not-set value-not-set value-not-set value-not-set))
           (dbus-value "Nameservers" props)
           (dbus-value "Domains" props)
           (or (and-let* ((proxy (dbus-value "Proxy" props))
                          (_ (and (not (empty-dbus-value? proxy))
                                  (not (value-not-set? proxy))))
                          (proxy (vector->list proxy)))
                 (make-proxy
                  (dbus-value "Method" proxy)
                  (dbus-value "Address" proxy)
                  (dbus-value "Netmask" proxy)
                  (dbus-value "Gateway" proxy)))
               (make-proxy value-not-set value-not-set value-not-set value-not-set))
           (or (and-let* ((provider (dbus-value "Provider" props))
                          (_ (and (not (empty-dbus-value? provider))
                                  (not (value-not-set? provider))))
                          (provider (vector->list provider)))
                 (make-provider
                  service-context
                  (dbus-value "Host" provider)
                  (dbus-value "Domain" provider)
                  (dbus-value "Name" provider)
                  (dbus-value "Type" provider)))
               (make-provider
                value-not-set value-not-set value-not-set value-not-set value-not-set))
           (dbus-value "Nameservers.Configuration" props)
           (or (and-let* ((ipv4-conf (dbus-value "IPv4.Configuration" props))
                          (_ (and (not (empty-dbus-value? ipv4-conf))
                                  (not (value-not-set? ipv4-conf))))
                          (ipv4-conf (vector->list ipv4-conf)))
                 (make-ipv4-configuration
                  service-context
                  (dbus-value "Method" ipv4-conf)
                  (dbus-value "Address" ipv4-conf)
                  (dbus-value "Netmask" ipv4-conf)
                  (dbus-value "Gateway" ipv4-conf)))
               (make-ipv4-configuration
                value-not-set value-not-set value-not-set value-not-set value-not-set))
           (or (and-let* ((ipv6-conf (dbus-value "IPv6.Configuration" props))
                          (_ (and (not (empty-dbus-value? ipv6-conf))
                                  (not (value-not-set? ipv6-conf))))
                          (ipv6-conf (vector->list ipv6-conf)))
                 (make-ipv6-configuration
                  service-context
                  (dbus-value "Method" ipv6-conf)
                  (dbus-value "Address" ipv6-conf)
                  (dbus-value "PrefixLength" ipv6-conf)
                  (dbus-value "Gateway" ipv6-conf)
                  (dbus-value "Privacy" ipv6-conf)))
               (make-ipv6-configuration
                value-not-set value-not-set value-not-set value-not-set value-not-set value-not-set))
           (or (and-let* ((proxy-conf (dbus-value "Proxy.Configuration" props))
                          (_ (and (not (empty-dbus-value? proxy-conf))
                                  (not (value-not-set? proxy-conf))))
                          (proxy-conf (vector->list proxy-conf)))
                 (make-proxy-configuration
                  service-context
                  (dbus-value "Method" proxy-conf)
                  (dbus-value "Address" proxy-conf)
                  (dbus-value "Netmask" proxy-conf)
                  (dbus-value "Gateway" proxy-conf)))
               (make-proxy-configuration
                value-not-set value-not-set value-not-set value-not-set value-not-set))
           (dbus-value "Domains.Configuration" props)
           )))))


(define (service-connect! context)
  (dbus-call context "Connect"))


(define (service-disconnect! context)
  (dbus-call context "Disconnect"))


(define (service-remove! context)
  (dbus-call context "Remove"))


(define (service-move-before! context)
  (dbus-call context "MoveBefore"))


(define (service-move-after! context)
  (dbus-call context "MoveAfter"))


(define (service-reset-counters! context)
  (dbus-call context "ResetCounters"))



;;;
;;; Use `advise' to update records _and_ call dbus methods
;;;

(define-syntax configuration-setter
  (syntax-rules ()
    ((_ section proc method context-getter)
     (advise 'before proc
             (lambda (args)
               (let* ((obj (car args))
                      (context (context-getter obj))
                      (new-val (cadr args))
                      (dbus-val (make-variant `#((,method . ,new-val)))))
                 (dbus-call context "SetProperty" section dbus-val)))))))

(define-syntax set-methods!
  (syntax-rules ()
    ((_ section context-getter procs/methods)
     (for-each
      (lambda (proc/method)
        (let ((proc (car proc/method))
              (method (cdr proc/method)))
          (configuration-setter section proc method context-getter)))
      procs/methods))))


;;; IPv6
(set-methods! "IPv6.Configuration"
              ipv6-configuration-context
              `((,ipv6-configuration-address-set! . "Address")
                (,ipv6-configuration-privacy-set! . "Privacy")
                (,ipv6-configuration-method-set!  . "Method")
                (,ipv6-configuration-gateway-set! . "Gateway")
                (,ipv6-configuration-prefix-length-set!  . "PrefixLength")))

;;; IPv4
(set-methods! "IPv4.Configuration"
              ipv4-configuration-context
              `((,ipv4-configuration-address-set! . "Address")
                (,ipv4-configuration-netmask-set! . "Netmask")
                (,ipv4-configuration-method-set!  . "Method")
                (,ipv4-configuration-gateway-set! . "Gateway")))


;;; Proxy
(set-methods! "Proxy.Configuration"
              proxy-configuration-context
              `((,proxy-configuration-method-set!   . "Method")
                (,proxy-configuration-url-set!      . "URL")
                (,proxy-configuration-servers-set!  . "Servers")
                (,proxy-configuration-excludes-set! . "Excludes")))


;;; Domains configuration
(advise 'before service-properties-domains-configuration-set!
        (lambda (args)
          (let* ((obj (car args))
                 (context (service-properties-context obj))
                 (new-val (cadr args))
                 (dbus-val (make-variant new-val)))
            (dbus-call context
                       "SetProperty"
                       "Domains.Configuration" dbus-val))))


;;; Nameservers configuration
(advise 'before service-properties-nameservers-configuration-set!
        (lambda (args)
          (let* ((obj (car args))
                 (context (service-properties-context obj))
                 (new-val (cadr args))
                 (dbus-val (make-variant new-val)))
            (dbus-call context
                       "SetProperty"
                       "Nameservers.Configuration" dbus-val))))
