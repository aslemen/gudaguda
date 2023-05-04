(in-package :gudaguda/test)

(def-suite* :gudaguda/io :in :gudaguda)


(defparameter *terms*
    (list (cons '(const 3) (make-const :data 3))
          (cons 3 (make-vari
           :data 3))
          (cons 'a (make-vari :data 'a))
          (cons '(τ (const 3) :type a)
                (make-type-annotation 
                    :annotated (make-const :data 3)
                    :type (make-vari :data 'a)
                )
          )
    )
)

(test parse-pseudo-expr
    (iter (for (raw . read) in *terms*)
        (is (equiv-terms (eval (parse-pseudo-expr raw)) read)
        )
    )

)

(test encode-pseudo-expr
    (iter (for (raw . read) in *terms*)
        (is (equalp (encode-term read) raw)
        )
    )
)