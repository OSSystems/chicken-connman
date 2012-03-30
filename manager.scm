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
  (dbus-call
   (manager-properties-context properties)
   "SetProperty"
   "OfflineMode"
   (make-variant offline?))
  (manager-properties-offline-mode?-set! properties offline?))


(define (manager-properties-set-session-mode! properties session-mode?)
  (dbus-call
   (manager-properties-context properties)
   "SetProperty"
   "SessionMode"
   (make-variant session-mode?))
  (manager-properties-session-mode?-set! properties session-mode?))


;;;
;;; Manager technologies
;;;
(define-record manager-technology
  path
  name
  type
  powered?
  connected?
  thetering?)

(define (manager-technologies context)
  (and-let* ((techs (dbus-call context "GetTechnologies"))
             (techs (vector->list (car techs))))
    (map (lambda (item)
           (let ((path (object-path->string (struct-ref item 0)))
                 (properties (vector->list (struct-ref item 1))))
             (make-manager-technology path
                                      (dbus-value "Name" properties)
                                      (dbus-value "Type" properties)
                                      (dbus-value "Powered" properties)
                                      (dbus-value "Connected" properties)
                                      (dbus-value "Thetering" properties))))
         techs)))


;;;
;;; Manager services
;;;
(define (manager-services context)
  (and-let* ((services (dbus-call context "GetServices"))
             (services (vector->list (car services))))
    (map (lambda (item)
           (let* ((path (object-path->string (struct-ref item 0)))
                  (context (make-service-context path)))
             (service-properties context)))
         services)))
