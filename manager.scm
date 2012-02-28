(define (make-manager-context)
  (dbus-make-context bus: system-bus
                     path: '/
                     service: 'net.connman
                     interface: 'net.connman.Manager))


(define (manager-state context)
  (and-let* ((state (dbus-call context "GetState")))
    (car state)))


(define (manager-technologies context)
  (and-let* ((techs (dbus-call context "GetTechnologies")))
    (car techs)))


(define (manager-services context)
  (and-let* ((services (dbus-call context "GetServices")))
    (car services)))