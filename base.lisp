(defpackage :gudaguda
  (:use :cl :iterate)
  (:nicknames :guda :gdgd)
  (:export
    #:base-error
    ;; ------
    ;; io.lisp
    ;; ------
    ;; conditions
    #:io-error #:parsing-error #:parsing-command-error
    ;; lambda expression keywords
    #:λ #:app #:τ #:lisp #:const
    #:bind #:infer #:reduce
    ;; readers
    #:read-term
    #:read-command
    #:read-script
    ;; context
    #:init-context
    #:context-assignments
    #:context-result
    ;; encoders
    #:encode-term
  )
  (:import-from :trivia
    :match :guard :place
  )
  (:import-from :trivia.level2.impl
    :plist :assoc :structure 
  )
  (:import-from :queues
    :make-queue :qpush
  )
)

(in-package :gudaguda)

(define-condition base-error (error)
  ()
)