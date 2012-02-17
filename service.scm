(define (connman-discover-services)
  ;; FIXME: handle errors (depends on dbus egg raising errors)
  (let* ((c (dbus-make-context bus: system-bus
                               service: 'net.connman
                               interface: 'org.freedesktop.DBus.Introspectable
                               path: '/net/connman/service))
         (result (xml->sxml (dbus-call c "Introspect")))
         (nodes (cdr result)))
    (map (lambda (node)
           (match node
             ((node (node (@ (_name name)))) name)))
         nodes)))


(define-record ethernet method interface address mtu speed duplex)
(define-record ipv4 method address netmask gateway)
(define-record ipv6 method address prefix-length gateway privacy)
(define-record proxy method url servers excludes)
(define-record ipv4-configuration context method address netmask gateway)
(define-record ipv6-configuration context method address prefix-length gateway privacy)
(define-record proxy-configuration context method url servers excludes)

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
  proxy-configuration)


(define (make-service-context service)
  (dbus-make-context bus: system-bus
                     service: 'net.connman
                     interface: 'net.connman.Service
                     path: (make-pathname "/net/connman/service"
                                          service)))


(define (service-properties service-context)
  (let (;; FIXME: handle errors (depends on dbus egg raising errors)
        (properties (dbus-call service-context "GetProperties")))
    (pp properties)
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
           (dbus-value "Provider" props)
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


;; Error out when attempting to set read-only properties
(for-each (lambda (proc)
            (advise 'before
                    (eval proc)
                    (lambda _
                      (error proc "This property is read-only."))))
          '(service-properties-state-set!
            service-properties-name-set!
            service-properties-type-set!
            service-properties-security-set!
            service-properties-favorite?-set!
            service-properties-immutable?-set!
            service-properties-auto-connect?-set!
            service-properties-login-required?
            service-properties-strength-set!
            service-properties-roaming?-set!
            service-properties-domains-set!
            service-properties-ipv4-set!
            service-properties-ipv6-set!
            service-properties-proxy-set!
            service-properties-provider-set!
            service-properties-ethernet-set!
            ethernet-method-set!
            ethernet-interface-set!
            ethernet-address-set!
            ethernet-mtu-set!
            ethernet-speed-set!
            ethernet-duplex-set!
            ipv4-method-set!
            ipv4-address-set!
            ipv4-netmask-set!
            ipv4-gateway-set!
            ipv6-method-set!
            ipv6-address-set!
            ipv6-prefix-length-set!
            ipv6-gateway-set!
            ipv6-privacy-set!
            proxy-method-set!
            proxy-url-set!
            proxy-servers-set!
            proxy-excludes-set!
            ))

;;; Use `advise' to update records _and_ call dbus methods (require
;;; dbus support for marshalling variants)
;;
;; (advise 'before ipv6-configuration-method-set!
;;         (lambda (args)
;;           (let* ((ipv6 (car args))
;;                 (context (ipv6-configuration-context ipv6))
;;                 (new-val (cadr args)))
;;             (dbus-call context "SetProperty" "IPv6.Configuration" new-val))))
