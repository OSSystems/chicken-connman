(define (make-manager-context)
  (dbus-make-context bus: system-bus
                     path: '/
                     service: 'net.connman
                     interface: 'net.connman.Manager))


;;;
;;; Manager properties
;;;
(define-record manager-properties
  context
  state
  offline-mode?
  available-debugs
  enabled-debugs
  session-mode?)

(define (manager-properties context)
  (and-let* ((properties (dbus-call context "GetProperties"))
             (properties (vector->list (car properties))))
    (make-manager-properties
     context
     (dbus-value "State" properties)
     (dbus-value "OfflineMode" properties)
     (dbus-value "AvailableDebugs" properties)
     (dbus-value "EnabledDebugs" properties)
     (dbus-value "SessionMode" properties))))


(define (manager-properties-set-offline-mode! properties offline?)
  (manager-properties-offline-mode?-set! properties offline?)
  (dbus-call
   (manager-properties-context properties)
   "SetProperty"
   "OfflineMode"
   (make-variant offline?)))


(define (manager-properties-set-session-mode! properties session-mode?)
  (manager-properties-session-mode?-set! properties session-mode?)
  (dbus-call
   (manager-properties-context properties)
   "SetProperty"
   "SessionMode"
   (make-variant session-mode?)))


(define (manager-technologies context)
  (and-let* ((techs (dbus-call context "GetTechnologies")))
    (car techs)))


(define (manager-services context)
  (and-let* ((services (dbus-call context "GetServices")))
    (car services)))
