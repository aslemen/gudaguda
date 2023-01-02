(in-package :gudaguda)

;; TODO implement constant
;; TODO implement let ... in
(defmacro read-term (s-expr)
  "Read S-EXPR, a lambda term encoded in a pseudo S-expression."
  (match s-expr
    ;; ------------
    ;; expression evaled by LISP
    ;; ------------
    ;; (lisp ...)
    ( (list 'lisp expr) expr )

    ;; ------------
    ;; canonical notations
    ;; ------------
    ;; constant
    ( (cons 'const (cons name (plist :type ty :level level)))
      `(make-const  :data   (quote ,name)
                    :type   (if ,ty (read-term ,ty))
                    :level  (or ,level 0)
      )
    )

    ;; variable
    ( (cons 'ζ (cons name (plist :type ty :level level)))
      `(make-vari   :data   (quote ,name)
                    :type   (if ,ty (read-term ,ty))
                    :level  (or ,level 0)
      )
    )

    ;; func
    ;; (λ (x) y)
    ( (cons 'λ (cons (cons argvari nil)
                     (cons conseq
                           (plist :type ty :level level)
                     )
               )
      )
      `(make-func :argvari  (read-term ,argvari)
                  :conseq   (read-term ,conseq)
                  :type     (if ,ty (read-term ,ty))
                  :level    (or ,level 0)
      )
    )

    ;; (app f x)
    ( (cons 'app 
            (cons functor
                  (cons arg 
                        (plist :type ty :level level)
                  )
            )
      )
      `(make-app  :functor  (read-term ,functor) 
                  :arg      (read-term ,arg)
                  :type     (if ,ty (read-term ,ty))
                  :level    (or ,level 0)
      )
    )

    ;; type annotation
    ;; (τ a :type ty :level level)
    ( (cons 'τ (cons annotated (plist :type ty :level level)))
      (let  ( (var-annot-read (gensym "ANNOT-READ"))
            )
        `(let ( (,var-annot-read (read-term ,annotated) )
              )
          (make-type-annotation
            :annotated ,var-annot-read
            :type     (if ,ty (read-term ,ty))
            :level    (or ,level (term-level ,var-annot-read))
          )
        )
      )
    )

    ( (cons 'assign 
        (cons (list openvari val) 
              (cons whole (plist :level level :type ty))
        )
      )
      `(make-assign
        :openvari (read-term ,openvari)
        :val      (read-term ,val)
        :whole    (read-term ,whole)
        :type     (if ,ty (read-term ,ty))
        :level    (or ,level 0)
      )
    )
    ;; ------------
    ;; non-canonical notations
    ;; ------------

    ;; func with multiple arg variables
    ;; (λ (x y z ...) ...)
    ( (cons 'λ (cons (list argvar1 argvari-rest)
                     (cons conseq plists)
               )
      )
      ;; normalize the form
      ;; direct to (λ (x) (λ (y z ...) ...) )
      `(read-term 
          (λ (,argvar1) 
             (λ ,argvari-rest ,conseq)
          )
          ,@plists
      )
    )

    ;; assignitution
    ;; ( (openvar ↦ val) whole ...)
    ( (cons (list openvar '↦ val)
            (cons whole plists)
      )
      ;; normalize the form
      ;; direct to (assign (openvar val) whole ...)
      `(read-term
        (assign (,openvar ,val) ,whole ,@plists)
      )
    )

    ;; variable with type annotation
    ;; (x :type ty :level level)
    ( (guard (cons singleton (plist :type ty :level level))
             (not (and (null ty) (null level)))
      )
      `(make-vari :data (quote ,singleton) 
                  :type (if ,ty (read-term ,ty))
                  :level (or ,level 0)
      )
    )
    ;; Lisp-style function application
    ;; (f x y z ...)
    ( (cons functor (cons _ _) )
      ;; normalize the form
      ;; direct to ((app (app (app f x) y) z) ...)
      (iter 
        (with f = functor)
        (for arg in (cdr s-expr))

        (setq f `(app ,f ,arg))

        (finally (return `(read-term ,f)) )
      )
    )

    ;; superfluous parentheses
    ;; (x)
    ( (cons singleton nil)
      ;; just unwrap the superfluous parentheses
      ;; direct to x
      `(read-term ,singleton)
    )

    ( otherwise
      `(make-vari :data (quote ,s-expr))
    )
  )
)

(defstruct (context)
  (assignments    nil :type list)
  (result         nil :type queues:simple-queue)
)

(defun init-context (&key (assignments nil) (result nil))
  (make-context :assignments assignments
                :result (if (null result)
                            (make-queue :simple-queue)
                            result
                        )
  )
)

(defmacro read-command ((ctx) &body comm)
  (match comm
    ( (list 'bind vari obj)
      `(push (cons (quote ,vari) (read-term ,obj) )
             (context-assignments ,ctx)
      )
    )
    ( (list 'infer obj)
      `(qpush (context-result ,ctx) 
              (reduce-term (read-term ,obj)
                :assignments (context-assignments ,ctx)
                :do-beta nil
              )
      )
    )
    ( (list 'reduce obj) 
      `(qpush (context-result ,ctx)
              (reduce-term (read-term ,obj)
                  :assignments (context-assignments ,ctx)
                  :do-beta t
              )
      )
    )
    ( otherwise
      (error "INCORRECT SYNTAX")
    )
  )
)

(defmacro read-script ( (ctx) &body body)
  `(progn ,@(mapcar #'(lambda (line)
                        `(read-command (,ctx) ,@line)
                      )
                      body
          )
  )
)

(declaim (ftype (function (term) list) encode-term))
(defgeneric encode-term (obj) )

(defmethod encode-term ( (obj vari) )
  (let  ( (children-encoded (maplist-term #'encode-term obj))
          (data (atomic-data obj))
          (level (term-level obj))
        )
    (match children-encoded
      ( (list nil)
        (cond 
          ( (= 0 level) data )
          ( t 
            `(,data :level ,level)
          )
        )
      )
      ( (list ty-encoded) 
        (cond 
          ( (= 0 level) 
            `(,data :type ,ty-encoded )
          )
          ( t 
            `(,data :type ,ty-encoded :level ,level)
          )
        )
      )
    )
  )
)

(defmethod encode-term ( (obj const) )
  (let  ( (children-encoded (maplist-term #'encode-term obj))
          (data (atomic-data obj))
          (level (term-level obj))
        )
    (match children-encoded
      ( (list nil)
        (cond 
          ( (= 0 level)
            `(const ,data)
          )
          ( t 
            `(const ,data :level ,level)
          )
        )
      )
      ( (list ty-encoded) 
        (cond 
          ( (= 0 level) 
            `(const ,data :type ,ty-encoded )
          )
          ( t 
            `(const ,data :type ,ty-encoded :level ,level)
          )
        )
      )
    )
  )
)

(defmethod encode-term ((obj func) )
  (let  ( (children-encoded (maplist-term #'encode-term obj))
          (level (term-level obj))
        )
    (match children-encoded 
      ( (list argvari-encoded conseq-encoded nil)
        (cond  
          ( (= 0 level)
            `(λ (,argvari-encoded) ,conseq-encoded)
          )
          ( t 
            `(λ (,argvari-encoded) ,conseq-encoded :level ,level)
          )
        )
      )
      ( (list argvari-encoded conseq-encoded ty-encoded)
        (cond
          ( (= 0 level)
            `(λ (,argvari-encoded) ,conseq-encoded
                :type ,ty-encoded
            )
          )
          ( t 
            `(λ (,argvari-encoded) ,conseq-encoded
                :type ,ty-encoded
                :level ,level
            )
          )
        )
      )
    )
  )
)

(defmethod encode-term ((obj app) )
  (let  ( (children-encoded (maplist-term #'encode-term obj))
          (level (term-level obj))
        )
    (match children-encoded 
      ( (list functor-encoded arg-encoded nil)
        (cond  
          ( (= 0 level)
            `(app ,functor-encoded ,arg-encoded)
          )
          ( t 
            `(app ,functor-encoded ,arg-encoded :level ,level)
          )
        )
      )
      ( (list functor-encoded arg-encoded ty-encoded)
        (cond
          ( (= 0 level)
           `(app ,functor-encoded ,arg-encoded
                :type ,ty-encoded
            )
          )
          ( t 
            `(app ,functor-encoded ,arg-encoded
                :type ,ty-encoded
                :level ,level
            )
          )
        )
      )
    )
  )
)

(defmethod encode-term ((obj type-annotation) )
  (let  ( (children-encoded (maplist-term #'encode-term obj))
          (level (term-level obj))
        )
    (match children-encoded 
      ( (list annotated-encoded nil)
        (cond  
          ( (= 0 level) 
            `(τ ,annotated-encoded)
          )
          ( t 
            `(τ ,annotated-encoded :level ,level)
          )
        )
      )
      ( (list annotated-encoded ty-encoded)
        (cond
          ( (= 0 level)
            `(τ ,annotated-encoded :type ,ty-encoded)
          )
          ( t 
            `(τ ,annotated-encoded
                :type ,ty-encoded
                :level ,level
            )
          )
        )
      )
    )
  )
)
(defmethod encode-term ((obj assign))
  (let  ( (children-encoded (maplist-term #'encode-term obj))
          (level (term-level obj))
        )
    (match children-encoded 
      ( (list openvari-encoded val-encoded whole-encoded nil)
        (cond  
          ( (= 0 level) 
            `(assign (,openvari-encoded ,val-encoded) ,whole-encoded)
          )
          ( t 
            `(assign (,openvari-encoded ,val-encoded) ,whole-encoded
                    :level ,level
            )
          )
        )
      )
      ( (list openvari-encoded val-encoded whole-encoded ty-encoded)
        (cond
          ( (= 0 level)
            `(assign (,openvari-encoded ,val-encoded) ,whole-encoded
                    :type ,ty-encoded
            )
          )
          ( t 
            `(assign (,openvari-encoded ,val-encoded) ,whole-encoded
                    :type ,ty-encoded :level ,level
            )
          )
        )
      )
    )
  )
)

(defmethod print-object ((obj term) out)
  (print-unreadable-object (obj out)
    (print-object (encode-term obj) out)
  )
)