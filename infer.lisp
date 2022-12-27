(in-package :gudaguda)

(declaim (ftype (function (term) term) erase-types) )
(defun erase-types (obj)
  (match obj 
    ( (structure type-annotation :annotated an)
      (erase-types an)
    )
    ( (structure term)
      (let  ( (obj-copied (map-term #'erase-types obj))
      
            )
        (setf (term-type obj-copied) nil)
        ;; return
        obj-copied
      )
    )
  )
)

(declaim (ftype (function (term) term) fill-unspecified-types))
(defun fill-unspecified-types (obj)
  (let  ( (obj-copied (map-term #'fill-unspecified-types obj))
        )
    (match obj-copied
      ( (structure term :type nil :level level)
        (setf (term-type obj-copied) 
              (make-vari :data (gensym "TYPEVAR_") :level (1+ level))
        )
      )
    )
    ;; return 
    obj-copied
  )
)

(declaim (ftype (function * null) elab-term-info))
(defun elab-term-info (obj &key (test #'eq))
  (declare  (type term obj)
            (type (function (t t) boolean) test)
  )
  ;; bottom-up elaboration
  ;; modified in-situ
  (maplist-term #'elab-term-info obj :test test)

  ;; TODO: collect type infos of different occurrences of vari and const
  (match obj
    ( (structure vari :data at 
        :type (structure term :constrs   (place constrs-ty)
                              :assignments assignments-ty
                              :free-vars free-vars-ty
              )
        :constrs    (place constrs)
        :assignments  (place assignments)
        :free-vars  (place free-vars)
      )

      ;; merge freevars and constraints
      (setf constrs (append constrs constrs-ty))
      (push (cons (make-vari-typevari :data at)
                  (term-type obj)
            )
            constrs
      )
      (setf free-vars (append free-vars free-vars-ty))
      ;; constraint: (TYPEvari . at) ≡ type
      (setf assignments (append assignments assignments-ty))

      ;; remove caches
      (setf constrs-ty nil)
    )

    ( (structure const :data at
        :type (structure term :constrs (place constrs-ty)
                              :assignments assignments-ty
                              :free-vars free-vars-ty
              )
        :constrs    (place constrs)
        :assignments  (place assignments)
        :free-vars  (place free-vars)
      )

      ;; merge freevars and constraints
      (setf constrs (append constrs constrs-ty))
            (push (cons (make-vari-typevari :data at)
                  (term-type obj)
            )
            constrs
      )
      (setf assignments (append assignments assignments-ty))
      (setf free-vars (append free-vars free-vars-ty))

      ;; remove caches
      (setf constrs-ty nil)
    )

    ( (structure func
        :argvari (structure vari :data at
                                :type type-argvari
                                :constrs  (place constrs-argvari)
                                :assignments assignments-argvari
                                :free-vars free-vars-argvari
                )
        :conseq (structure term :constrs  (place constrs-conseq)
                                :assignments assignments-conseq
                                :free-vars free-vars-conseq
                )
        :level    level
        :type   (structure term :constrs  (place constrs-ty)
                                :assignments assignments-ty
                                :free-vars free-vars-ty
              )
        :constrs (place constrs)
        :constrs-local (place constrs-local)
        :assignments (place assignments)
        :free-vars (place free-vars)
      )
      ;; merge freevars and constraints
      (setf free-vars 
          (append free-vars
                  free-vars-argvari
                  free-vars-conseq 
                  free-vars-ty
          )
      )
      (setf constrs
          (append constrs 
                  constrs-argvari
                  constrs-conseq 
                  constrs-ty
          )
      )
      ;; filter out local constraints
      ;; since the argvari (of name AT) is not free anymore
      (match (classify constrs 
                #'(lambda (equiv)
                    (match equiv 
                      ( (cons (structure term :free-vars fv-left) 
                              (structure term :free-vars fv-right)
                        )
                        (or (find at fv-left  :test test )
                            (find at fv-right :test test )
                        )
                      )
                    )
                )
              )
        ( (cons constrs-with-at constrs-wo-at)

          (setf constrs constrs-wo-at)
          (setf constrs-local constrs-with-at)
        )
      )

      ;; add constraint: type of obj ≡ λ argvar, type of conseq
      (push (cons (term-type obj) 
                  (make-func 
                    :argvari (func-argvari obj)
                    :conseq (term-type (func-conseq obj))
                    :level (1+ level)
                  )
              )
              constrs
      )

      (setf assignments
            (append assignments 
                    assignments-argvari
                    assignments-conseq 
                    assignments-ty
            )
      )

      ;; remove argvar
      (setf free-vars (remove at free-vars :test test) )

      ;; delete caches
      (setf constrs-argvari nil)
      (setf constrs-conseq nil)
      (setf constrs-ty nil)
    )

    ( (structure app
        :functor (structure term  :type type-functor
                                  :constrs (place constrs-functor)
                                  :assignments assignments-functor
                                  :free-vars free-vars-functor
                  )
        :arg      (structure term :type type-arg
                                  :constrs (place constrs-arg)
                                  :assignments assignments-arg
                                  :free-vars free-vars-arg
                  )
        :level      level
        :type     (structure term :constrs (place constrs-ty)
                                  :assignments assignments-ty
                                  :free-vars free-vars-ty
                  )
        :constrs   (place constrs)
        :assignments (place assignments)
        :free-vars (place free-vars)
      )

      ;; merge freevars and constraints
      (setf free-vars
            (append free-vars
                    free-vars-functor
                    free-vars-arg
                    free-vars-ty
            )
      )

      (setf constrs
            (append constrs 
                    constrs-functor
                    constrs-arg 
                    constrs-ty
            )
      )
      ;; type of functor ≡ (λ x: type of arg). type of the whole
      (push (cons 
              type-functor 
              (make-func
                :argvari (make-vari :data (gensym "VAR_") :type type-arg)
                :conseq (term-type obj)
                :level 1000000
              )
            )
            constrs
      )
      (match type-functor
        ( (structure func :argvari (structure term :type type-argvari)
                          :conseq type-conseq
          )
          ;; type of argvari ≡ type of arg
          (push (cons type-argvari type-arg) constrs)
          ;; type of conseq ≡ type of the whole
          (push (cons type-conseq (term-type obj)) constrs)
        )

      )

      (setf assignments
            (append assignments
                    assignments-functor
                    assignments-arg 
                    assignments-ty
            )
      )

      ;; delete caches
      (setf constrs-functor nil)
      (setf constrs-arg nil)
      (setf constrs-ty nil)
    )

    ( (structure type-annotation
        :annotated  (structure term :type type-annotated
                                    :constrs (place constrs-annotated)
                                    :assignments assignments-annotated
                                    :free-vars free-vars-annotated
                    )
        :level      level
        :type     (structure term :constrs (place constrs-ty)
                                  :assignments assignments-ty
                                  :free-vars free-vars-ty
                  )
        :constrs  (place constrs)
        :assignments (place assignments)
        :free-vars (place free-vars)
      )

      ;; merge freevars and constraints
      (setf free-vars
            (append free-vars
                    free-vars-annotated
                    free-vars-ty
            )
      )

      (setf (term-constrs obj)
            (cons 
              ;; (type of annotated) ≡ type of the whole
              (cons type-annotated (term-type obj))
              (append constrs 
                      constrs-annotated
                      constrs-ty
              )
            )
      )
      (setf assignments 
            (append assignments 
                    assignments-annotated
                    assignments-ty
            )
      )

      ;; delete cahces
      (setf constrs-annotated nil)
      (setf constrs-ty nil)

    )
  )

  ;; return
  nil
)

(declaim (ftype (function * null) unify-constrs))
(defun unify-constrs (obj &key (test #'eq) (temp-constrs nil))
  (declare  (type term obj)
            (type (function (t t) boolean) test)
            (type list temp-constrs)
  )
  ;; bottom-up unification
  ;; modified in-situ
  (maplist-term #'unify-constrs obj :test test)

  (match obj
    ( (structure term :constrs constrs
                      :constrs-local constrs-local
                      :free-vars (place free-vars)
                      :assignments (place assignments)
      )
      (let  ( (constrs-temp (append constrs-local constrs))
            )
        (iter (while constrs-temp)
              (for equiv = (pop constrs-temp))

          ;; update equivalence cell
          (match equiv
            ( (cons nil _) 
              ;; do nothing
            )
            ( (cons _ nil)
              ;; do nothing
            )
            ( (cons (place left) (place right))
              ;; (break "EQUIV ~A ~A ~%" left right )
              (setf left  (reduce-term  left
                                        :assignments assignments
                                        :do-beta nil
                          )
              )
              (elab-term-info left :test test)

              (setf right (reduce-term  right
                                        :assignments assignments
                                        :do-beta nil
                          )
              )
              (elab-term-info right :test test)
            )
          )
          
          (match equiv
            ( (cons nil _)
              ;; nothing happens
            )
            ( (cons _ nil)
              ;; nothing happens
            )
            ( (cons (structure vari :data left :type ty-left)
                    (structure vari :data right :type ty-right)
              )
              ;; add constraint: type of vari-left ≡ type of vari-right
              (push (cons ty-left ty-right) constrs-temp)

              (cond 
                ( (funcall test left right)
                  ;; X = X
                  ;; do nothing
                )

                ( t 
                  ;; add left vari name ↦ right
                  (push (cons left (cdr equiv)) assignments)
                )
              )
            )

            ( (cons (structure const :data at-left  :type ty-left)
                    (structure const :data at-right :type ty-right)
              )

              (cond
                ( (funcall test at-left at-right)
                  ;; two constants are identical
                  ;; no problem

                  ;; add constraint: type of vari-left ≡ type of vari-right
                  (push (cons ty-left ty-right) constrs-temp)
                )
                ( t 
                  (error (format nil "Unmatched constants: ~a and ~a"
                                at-left at-right 
                          )
                  )
                )
              )
            )

            ( (cons (structure func
                  :argvari (structure vari :data vari-name-left :type ty-vari-left)
                  :conseq conseq-left
                  :type ty-left
                )
                (structure func
                  :argvari (structure vari :data vari-name-right :type ty-vari-right)
                  :conseq conseq-right
                  :type ty-right
                )
              )

              (let* ( (vari-dummy  (make-vari ;; how about CONST?
                                    :data (gensym "VAR_DUMMY_")
                                    :level 100000
                                    :type ty-vari-left
                                  )
                      )
                    )

                ;; add constraint: type of left 
                ;;                 ≡ type of right
                (push (cons ty-left ty-right) constrs-temp)

                ;; add constraint: type of argvari of left 
                ;;                 ≡ type of argvari of right
                (push (cons ty-vari-left ty-vari-right) constrs-temp)

                (push (cons (reduce-term conseq-left
                              :test test
                              :assignments  (cons (cons vari-name-left vari-dummy) 
                                                assignments
                                          )
                              :do-beta nil
                            )
                            (reduce-term conseq-right
                              :test test
                              :assignments  (cons (cons vari-name-right vari-dummy) 
                                                assignments
                                          )
                              :do-beta nil
                            )
                      )
                      constrs-temp
                )
              )
            )

            ( (cons (structure vari :data vari-name) 
                    (structure term :free-vars free-vars-right)
              )
              (cond 
                ;; if vari-name ∈ free-vars of right
                ( (find vari-name free-vars-right :test test)
                  (error (format nil "looping variable: ~a" vari-name) )
                )
                ;; if vari-name ∉ free-vars of right
                ( t 
                  ;; vari-name ↦ right
                  (push (cons vari-name (cdr equiv)) assignments)
                )
              )
            )

            ( (cons (structure term :free-vars free-vars-left)
                    (structure vari :data vari-name) 
              )

              (cond 
                ;; if vari-name ∈ free-vars of left
                ( (find vari-name free-vars-left :test test)
                  (error (format nil "looping variable: ~a" vari-name) )
                )
                ;; if vari-name ∉ free-vars of right
                ( t 
                  ;; vari-name ↦ right
                  (push (cons vari-name (car equiv)) assignments)
                )
              )
            )

            ( (cons (structure type-annotation :annotated an) 
                    right
              )
              (push (cons an right) constrs-temp)
            )

            ( (cons left 
                    (structure type-annotation :annotated an) 
              )
              (push (cons left an) constrs-temp)
            )

            ( otherwise
              (error (format nil "Illegal type: ~a" equiv) )
            )
          )

          ;; replace vari with the updateve assignments
          ;; in order to ensure transitivity of constraints
          (setf constrs-temp
            (mapcar #'(lambda (equiv) 
                        (cons (reduce-term  (car equiv)
                                            :test test
                                            :assignments assignments
                                            :do-beta nil
                              )
                              (reduce-term  (cdr equiv)
                                            :test test
                                            :assignments assignments
                                            :do-beta nil
                              )
                        )
                    )
                    constrs-temp
            )
          )
        )
      )
    )
  )

  ;; return
  nil
)

(declaim (ftype (function * null) update-assignments))
(defun update-assignments (obj &key (parent-assignments nil))
  (declare  (type term obj)
            (type list parent-assignments)
  )

  (setf (term-assignments obj) 
        (append (term-assignments obj) parent-assignments)
  )
  (maplist-term #'update-assignments obj 
               :parent-assignments (term-assignments obj)
  )
)

(declaim (ftype (function * null) infer-types))
(defun infer-types (obj &key (test #'eq))
  (declare  (type term obj)
            (type (function (t t) boolean) test)
  )
  (elab-term-info  obj :test test)
  (unify-constrs    obj :test test)
  (update-assignments obj)
)