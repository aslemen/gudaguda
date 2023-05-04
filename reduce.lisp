(in-package :gudaguda)

(declaim (ftype (function * (or term null))
                reduce-term
         )
)
(defgeneric reduce-term (obj &key test assignments do-beta)
  (:documentation "Substitute values for free variables in OBJ.

ASSIGNMENTS is an association list
whose keys are bare name of variables unwrapped from structure VAR,
and whose values are an arbitrary data of structure TERM.

If DO-BETA, the function will do β-reduction at the same time.

A new term object is returned and OBJ will not be tampered.
")
)
(defmethod reduce-term
    ( (obj const) &key (test #'eq) 
                       (assignments nil)
                       (do-beta nil)
    )
  (declare (type (function (t t) boolean) test)
           (type list assignments)
           (type boolean do-beta)
  )
  (match obj
    ( (structure const :data at :level level :type ty
                :assignments assignments
      )
      (make-const :data at :level level
        :type (reduce-term ty 
                :test test 
                :assignments assignments
                :do-beta do-beta
              )
        :assignments assignments
      )
    )
  )
)
(defmethod reduce-term
    ( (obj vari) &key (test #'eq) 
                      (assignments nil)
                      (do-beta nil)
    )
  (declare (type (function (t t) boolean) test)
           (type list assignments)
           (type boolean do-beta)
  )
  (match obj
    ( (structure vari :data at :level level :type ty
                      :assignments assignments-inherent
      )

      (match  (assoc  at
                      (append assignments assignments-inherent)
                      :test test
              )
        ;; if substitite for vari name AT is not found
        ( (or (cons _ nil) nil )
          ;; just do recursion
          (map-term  #'reduce-term obj 
                    :test test 
                    :assignments assignments 
                    :do-beta do-beta
          )

        )

        ;; if substitite is found
        ( (cons _ found-val)
          ;; return
          (make-type-annotation
            ;; replace the variable
            :annotated  (reduce-term found-val
                          :test test 
                          :assignments assignments
                          :do-beta do-beta
                        )
            :level        level
            :type       (reduce-term ty
                          :test test 
                          :assignments assignments
                          :do-beta do-beta
                        )
          )
        )
        ;; ( otherwise (error "ASSIGNMENTS must be an alist"))
      )
    )
  )
)
(defmethod reduce-term
    ( (obj func) &key (test #'eq) 
                      (assignments nil)
                      (do-beta nil)
    )
  (declare (type (function (t t) boolean) test)
        (type list assignments)
        (type boolean do-beta)
  )
  (match obj
    ( (structure func :argvari (structure vari :data at))
      ;; bare function
      ;; NOTE: β-reduction case does not come here but to the APP case
      (map-term  #'reduce-term obj 
                :test test
                ;; mask the bound variable
                :assignments (cons (cons at nil) assignments)
                :do-beta do-beta
      )
    )
  )
)
(defmethod reduce-term
    ( (obj app)  &key (test #'eq) 
                      (assignments nil)
                      (do-beta nil)
    )
  (declare (type (function (t t) boolean) test)
        (type list assignments)
        (type boolean do-beta)
  )
  (let  ( (obj-pre-reduced 
              (map-term #'reduce-term obj
                  :test test
                  :assignments assignments
                  :do-beta do-beta
              ) 
          )
        )
    (match obj-pre-reduced
      ( (guard  (structure app 
                  :functor  (structure func
                              :argvari (structure vari :data vari-name)
                              :conseq conseq
                            )
                  :arg arg
                  :level level
                  :type ty
                )
                do-beta ;; only when β-reduction is ON
        )
        ;; do β-reduction
        ;; [λ (x), conseq] arg 
        ;; ~~> [x ↦ arg] conseq
        (make-type-annotation
          :annotated  (reduce-term conseq
                        :test test
                        :assignments
                          (cons (cons vari-name arg) assignments)
                        :do-beta do-beta
                      )
          :level level
          :type ty
        )
      )

      ( (structure app 
          :functor  (structure type-annotation 
                      :annotated an
                      :level level-an
                    )
          :arg arg :level level :type ty
        )
        (make-type-annotation
          :annotated 
            (reduce-term 
              (make-app :functor an :arg arg
                        :level level-an
              )
              :test test
              :assignments assignments
              :do-beta do-beta
            )
          :level level
          :type ty
        )
      )

      ( otherwise 
        ;; β-reduction is disallowed or impossible
        ;; return
        obj-pre-reduced
      )
    )
  )
)
(defmethod reduce-term
      ( (obj term) &key (test #'eq)
                        (assignments nil)
                        (do-beta nil)
      )
  (declare (type (function (t t) boolean) test)
        (type list assignments)
        (type boolean do-beta)
  )
  ;; just do recursion
  (map-term  #'reduce-term obj 
            :test test 
            :assignments assignments 
            :do-beta do-beta
  )
)
(defmethod reduce-term
  ( (obj null)  &key (test #'eq) 
                     (assignments nil)
                     (do-beta nil)
  )
  (declare (type (function (t t) boolean) test)
          (type list assignments)
          (type boolean do-beta)
  )
  nil
)

(declaim (ftype (function (term) term) remove-type-annotations))
(defun remove-type-annotations (obj)
"Remove all type annotations of OBJ."
  (match obj
    ( (structure type-annotation :annotated an)
      (remove-type-annotations an)
    )
    ( (structure term) 
      (map-term #'remove-type-annotations obj)
    )
    ( otherwise obj )
  )
)