(use ssax matchable posix advice)
(use (rename dbus ;; Important: dbus 0.90 is required
             (call dbus-call)
             (make-context dbus-make-context)))

;; FIXME: use include when compiling
(load-relative "service.scm")
(load-relative "manager.scm")


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
