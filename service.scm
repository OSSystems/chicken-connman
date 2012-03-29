(define (connman-discover-services)
  (or (and-let* ((c (dbus-make-context
                     bus: system-bus
                     service: 'net.connman
                     interface: 'org.freedesktop.DBus.Introspectable
                     path: '/net/connman/service))
                 (introspect-results (handle-exceptions exn
                                       #f
                                       (dbus-call c "Introspect")))
                 (result (xml->sxml introspect-results))
                 (nodes (cdar (cdr result))))
        (map (lambda (node)
               (match node
                 ((node (@ (_name name))) name)))
             nodes))
      '()))


(define-record ethernet method interface address mtu speed duplex)
(define-record ipv4 method address netmask gateway)
(define-record ipv6 method address prefix-length gateway privacy)
(define-record proxy method url servers excludes)
(define-record ipv4-configuration context method address netmask gateway)
(define-record ipv6-configuration context method address prefix-length gateway privacy)
(define-record proxy-configuration context method url servers excludes)
(define-record provider context host domain name type)

(define-record service-properties
  path
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
                     path: (if (absolute-pathname? (->string service))
                               service
                               (make-pathname "/net/connman/service"
                                              service))))


(define (service-properties service-context)
  (let ((properties (handle-exceptions exn
                      #f
                      (dbus-call service-context "GetProperties"))))
    (if (or (not properties) (null? properties))
        #f
        (let* ((props (vector->list (car properties))))
          (make-service-properties
           (->string (vector-ref service-context 3))
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
;;; Setters
;;;

;;;
;;; AutoConnect
;;;
(define (service-properties-set-auto-connect! properties auto-connect?)
  (service-properties-auto-connect?-set! properties auto-connect?)
  (dbus-call
   (service-properties-context properties)
   "SetProperty"
   "AutoConnect"
   (make-variant auto-connect?)))


;;;
;;; IPv4
;;;
(define (ipv4-configuration-set-dhcp! ipv4-config)
  (ipv4-configuration-method-set! ipv4-config "dhcp")
  (dbus-call
   (ipv4-configuration-context ipv4-config)
   "SetProperty"
   "IPv4.Configuration"
   (make-variant `#(("Method" . ,(make-variant "dhcp"))))))


(define (ipv4-configuration-set-off! ipv4-config)
  (ipv4-configuration-method-set! ipv4-config "off")
  (dbus-call
   (ipv4-configuration-context ipv4-config)
   "SetProperty"
   "IPv4.Configuration"
   (make-variant `#(("Method" . ,(make-variant "off"))))))


(define (ipv4-configuration-set-manual! ipv4-config address #!key netmask gateway)
  (ipv4-configuration-method-set! ipv4-config "manual")
  (ipv4-configuration-address-set! ipv4-config address)
  (when netmask (ipv4-configuration-netmask-set! ipv4-config netmask))
  (when gateway (ipv4-configuration-gateway-set! ipv4-config gateway))
  (dbus-call
   (ipv4-configuration-context ipv4-config)
   "SetProperty"
   "IPv4.Configuration"
   (make-variant
    (list->vector
     (append
      `(("Method" . ,(make-variant "manual"))
        ("Address" . ,(make-variant address)))
      (if netmask
          `(("Netmask" . ,(make-variant netmask)))
          '())
      (if gateway
          `(("Gateway" . ,(make-variant gateway)))
          '()))))))

;;;
;;; IPv6
;;;
(define (ipv6-configuration-set-auto! ipv6-config)
  (ipv6-configuration-method-set! ipv6-config "auto")
  (dbus-call
   (ipv6-configuration-context ipv6-config)
   "SetProperty"
   "Ipv6.Configuration"
   (make-variant `#(("Method" . ,(make-variant "auto"))))))


(define (ipv6-configuration-set-off! ipv6-config)
  (ipv6-configuration-method-set! ipv6-config "off")
  (dbus-call
   (ipv6-configuration-context ipv6-config)
   "SetProperty"
   "Ipv6.Configuration"
   (make-variant `#(("Method" . ,(make-variant "off"))))))


(define (ipv6-configuration-set-manual! ipv6-config #!key address prefix-length gateway privacy)
  (ipv6-configuration-method-set! ipv6-config "manual")
  (when address (ipv6-configuration-address-set! ipv6-config address))
  (when prefix-length (ipv6-configuration-prefix-length-set! ipv6-config prefix-length))
  (when gateway (ipv6-configuration-gateway-set! ipv6-config gateway))
  (when privacy (ipv6-configuration-privacy-set! ipv6-config privacy))
  (let ((conf-group (list address prefix-length gateway)))
    ;; Either all or none set
    (when (and (any identity conf-group)
               (not (every identity conf-group)))
      (error 'ipv6-configuration-set-manual!
             "Either all or none of `address', `prefix-length' and `gateway' should be set.")))
  (dbus-call
   (ipv6-configuration-context ipv6-config)
   "SetProperty"
   "Ipv6.Configuration"
   (make-variant
    (list->vector
     (append
      `(("Method" . ,(make-variant "manual")))
      (if (and address prefix-length gateway)
          `(("Address" . ,(make-variant address))
            ("PrefixLength" . ,(make-variant prefix-length))
            ("Gateway" . ,(make-variant gateway)))
          '())
      (if privacy
          `(("Privacy" . ,(make-variant privacy)))
          '()))))))


;;;
;;; Proxy
;;;
(define (proxy-configuration-set-direct! proxy-config)
  (proxy-configuration-method-set! proxy-config "direct")
  (dbus-call
   (proxy-configuration-context proxy-config)
   "SetProperty"
   "Proxy.Configuration"
   (make-variant `#(("Method" . ,(make-variant "direct"))))))


(define (proxy-configuration-set-manual! proxy-config #!key servers excludes)
  (let ((servers (and servers (list->vector servers)))
        (excludes (and excludes (list->vector excludes))))
    (proxy-configuration-method-set! proxy-config "direct")
    (when servers (proxy-configuration-servers-set! proxy-config servers))
    (when excludes (proxy-configuration-excludes-set! proxy-config excludes))
    (dbus-call
     (proxy-configuration-context proxy-config)
     "SetProperty"
     "Proxy.Configuration"
     (make-variant
      (list->vector
       (append
        `(("Method" . ,(make-variant "direct")))
        (if servers
            `(("Servers" . ,(make-variant servers)))
            '())
        (if excludes
            `(("Excludes" . ,(make-variant excludes)))
            '())))))))


(define (proxy-configuration-set-auto! proxy-config url)
  (proxy-configuration-method-set! proxy-config "auto")
  (proxy-configuration-url-set! proxy-config url)
  (dbus-call
   (proxy-configuration-context proxy-config)
   "SetProperty"
   "Proxy.Configuration"
   (make-variant `#(("Method" . ,(make-variant "auto"))
                    ("URL" . ,(make-variant url))))))



;;;
;;; Use `advise' to update records _and_ call dbus methods
;;;


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
