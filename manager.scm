(define (make-manager-context)
  (dbus-make-context bus: system-bus
                     service: 'net.connman
                     interface: 'net.connman.Manager))


(define (manager-state context)
  (and-let* ((state (dbus-call context "GetState")))
    (car state)))
