(use connman posix)

;; The tests below are environment-specific, so we read the
;; CONNMAN_TESTS to make sure tests should be really run
(when (get-environment-variable "CONNMAN_TESTS")
  (let* ((services (connman-discover-services))
         (_ (print "Services: " services))
         (service-context (make-service-context (car services)))
         (manager-context (make-manager-context))
         (dp (service-properties service-context)))
    (print "===\n=== Manager\n===")
    (print "Manager State: " (manager-state manager-context))

    (print "\n===\n=== Service\n===")
    (print "Name: " (service-properties-name dp))
    (print "Type: " (service-properties-type dp))
    (print "Strength: " (service-properties-strength dp))
    (print "Immutable?: " (service-properties-immutable? dp))
    (print "Ethernet MTU: " (ethernet-mtu (service-properties-ethernet dp)))
    (print "Ethernet speed: " (ethernet-speed (service-properties-ethernet dp)))
    (print "IPv4 address: " (ipv4-address (service-properties-ipv4 dp)))
    (print "IPv6 address: " (ipv6-address (service-properties-ipv6 dp)))

    (handle-exceptions exn
      (print "Unable to set Name property: read-only (as expected)")
      (service-properties-name-set! dp "foo"))

    (print "Nameservers configuration: "
           (service-properties-nameservers-configuration dp))

    (print "IPv4 configuration: ")
    (let ((ipv4 (service-properties-ipv4-configuration dp)))
      (print "    Method: " (ipv4-configuration-method ipv4)))

    (print "\nSetting IPv6 configuration")
    (let ((ipv6 (service-properties-ipv6-configuration dp)))
      (print "IPv6 configuration (address): " (ipv6-configuration-address ipv6))
      (print "IPv6 configuration (privacy): " (ipv6-configuration-privacy ipv6))
      (print "IPv6 configuration (method): " (ipv6-configuration-method ipv6))
      (ipv6-configuration-method-set! ipv6 "auto")
      (print "IPv6 configuration (address): " (ipv6-configuration-address ipv6))
      (print "IPv6 configuration (privacy): " (ipv6-configuration-privacy ipv6))
      (print "IPv6 configuration (method): " (ipv6-configuration-method ipv6))
      )))
