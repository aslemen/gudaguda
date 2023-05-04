(defpackage :gudaguda/test
    (:use :cl :fiveam :iterate)
    (:import-from :gudaguda
        ;; symbols
        :const
        :τ
        ;; obj
        :make-term
        :make-atomic
        :make-vari
        :make-vari-typevari
        :make-const
        :make-app
        :make-func
        :make-type-annotation
        :equiv-terms
        ;; io
        :parse-pseudo-expr
        :read-pseudo-expr
        :encode-term
    )
)

(in-package :gudaguda/test)

(def-suite* :gudaguda)