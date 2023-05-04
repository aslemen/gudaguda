(in-package :gudaguda)

(define-condition io-error (base-error)
  ()
)

(define-condition parsing-error (io-error)
  ()
)

;; TODO implement constant
;; TODO implement let ... in
(defun parse-pseudo-expr (s-expr)
"Read a lambda term encoded in a pseudo-S-expr
and convert it to another S-expr 
which makes a term when evaluated."
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
                    :type   ,(if ty (parse-pseudo-expr ty))
                    :level  (or ,level 0)
      )
    )

    ;; variable
    ( (cons 'ζ (cons name (plist :type ty :level level)))
      `(make-vari   :data   (quote ,name)
                    :type   ,(if ty (parse-pseudo-expr ty))
                    :level  (or ,level 0)
      )
    )

    ;; func
    ;; (λ (x) y :type ty :level level)
    ( (cons 'λ (cons (cons argvari nil)
                     (cons conseq
                           (plist :type ty :level level)
                     )
               )
      )
      `(make-func :argvari  ,(parse-pseudo-expr argvari)
                  :conseq   ,(parse-pseudo-expr conseq)
                  :type     ,(if ty (parse-pseudo-expr ty))
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
      `(make-app  :functor  ,(parse-pseudo-expr functor) 
                  :arg      ,(parse-pseudo-expr arg)
                  :type     ,(if ty (parse-pseudo-expr ty))
                  :level    (or ,level 0)
      )
    )

    ;; type annotation
    ;; (τ a :type ty :level level)
    ( (cons 'τ (cons annotated (plist :type ty :level level)))
      (let* ( (var-annot-read (parse-pseudo-expr annotated))
            )
        `(make-type-annotation
          :annotated ,var-annot-read
          :type      ,(if ty (parse-pseudo-expr ty))
          :level     (or ,level (term-level ,var-annot-read))
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
      `(parse-pseudo-expr 
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
                  :type ,(if ty (parse-pseudo-expr ty))
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

        (finally (return (parse-pseudo-expr f)) )
      )
    )

    ;; superfluous parentheses
    ;; (x)
    ( (cons singleton nil)
      ;; just unwrap the superfluous parentheses
      ;; direct to x
      `(parse-pseudo-expr ,singleton)
    )

    ( otherwise
      `(make-vari :data (quote ,s-expr))
    )
  )
)

(defmacro read-pseudo-expr (&body s-expr)
  (parse-pseudo-expr s-exp)
)

(defstruct (context)
"Represents a evaluation context.

ASSIGNMENTS is an a-list of variable assignments.
RESULT is a queue that contains inference results.
"
  (assignments    nil :type list)
  (result         nil :type queues:simple-queue)
)

(defun init-context (&key (assignments nil) (result nil))
"Create a new context for a script."
  (make-context :assignments assignments
                :result (if (null result)
                            (make-queue :simple-queue)
                            result
                        )
  )
)

(define-condition parsing-command-error (parse-error)
  ( (input :type string)
  )
  (:report 
    (lambda (c stream)
      (format stream "Invalid command syntax: ~a~&" (input c))
    )
  )
  (:documentation "Error occurring when parsing a command.")
)

(defmacro read-command ((ctx) &body comm)
"Parse the LISP expression of a command and run it.

CTX provides the evaluating context.

List of commands:

* (bind VARI OBJ): bind OBJ to VARI and save it to the assignment of CTX.

* (infer OBJ): do type inference for OBJ.
  The result is pushed to the queue of CTX.

* (reduce OBJ): do B-reduction on OBJ and push the result to CTX.
"
  (match comm
    ( (list 'bind vari obj)
      `(push (cons (quote ,vari) ,(parse-pseudo-expr obj) )
             (context-assignments ,ctx)
      )
    )
    ( (list 'infer obj)
      `(qpush (context-result ,ctx) 
              (infer-types ,(parse-pseudo-expr obj)
              )
      )
    )
    ( (list 'reduce obj) 
      `(qpush (context-result ,ctx)
              (reduce-term ,(parse-pseudo-expr obj)
                :assignments (context-assignments ,ctx)
                :do-beta t
              )
      )
    )
    ( otherwise
      (error (make-condition 'parsing-command-error :input comm))
    )
  )
)

(defmacro read-script ( (ctx) &body body)
"
Parse a script and run it.

CTX is a context.
"
  `(progn ,@(mapcar #'(lambda (line)
                        `(read-command (,ctx) ,@line)
                      )
                      body
          )
  )
)

(declaim (ftype (function (term) list) encode-term))
(defgeneric encode-term (obj) 
  (:documentation
"Generate the pseudo S-expr representation of term OBJ."
  )
)

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