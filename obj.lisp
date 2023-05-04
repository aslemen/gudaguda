(in-package :gudaguda)

;; ============
;; Data structure declarations
;; ============

(defstruct term
"The base structure for terms.

LEVEL is the hierarchical level that this term belongs to.
Ordinary terms (numbers, individuals, etc.) are in level 0.
Types of them are in level 1.
Types of types of them are in level 2.

TYPE is the type annotation for this term, which is optional.

CONSTRS, CONSTRS-LOCAL, ASSIGNMENTS, FREE-VARS are
for internal caching used in type checking and β-reduction.
"
  (level            0   :type (integer 0))
  (type             nil :type (or null term))
  (constrs          nil :type list)
  (constrs-local    nil :type list)
  (assignments      nil :type list)
  (free-vars        nil :type list)
)

(defstruct (atomic        (:include term))
"The base structure for atomic terms."
  (data nil :type t)
)

(defstruct (const         (:include atomic))
"The structure for constants."
)
(defstruct (vari          (:include atomic))
"The structure for variables."
)
(defstruct (vari-typevari (:include vari))
"The structure for type variables."
)

(defstruct (func (:include term))
"The structure for lambda functions.

ARGVARI is a variable to be abstracted.
CONSEQ is the body of the lambda.

Schematically, λ ARGVARI, CONSEQ.
"
  (argvari nil :type vari)
  (conseq  nil :type term)
)

(defstruct (app (:include term))
"The structure for functional applications.

Schematically, (FUNCTOR ARG).
"
  (functor nil :type term)
  (arg     nil :type term)
)

(defstruct (type-annotation (:include term))
"The structure for type annotations.

ANNOTATED is the annotated term.
The type annotation goes to TYPE.
"
  (annotated nil :type term)
)

;; ============
;; Superficial equivalence
;; ============
(defgeneric equiv-terms (left right)
  (:documentation "Check the superficial equivalence of terms LEFT and RIGHT.")
)


(defmethod equiv-terms ((a atomic) (b atomic))
  (and (equal (atomic-data a) (atomic-data b))
       (eq (term-level a) (term-level b))
       (equiv-terms (term-type a) (term-type b))
  )
)
(defmethod equiv-terms ((a func) (b func))
  (and (equiv-terms (func-argvari a) (func-argvari b))
       (equiv-terms (func-conseq a) (func-conseq b))
       (eq (term-level a) (term-level b))
       (equiv-terms (term-type a) (term-type b))
  )
)
(defmethod equiv-terms ((a app) (b app))
  (and (equiv-terms (app-functor a) (app-functor b))
       (equiv-terms (app-arg a) (app-arg b))
       (eq (term-level a) (term-level b))
       (equiv-terms (term-type a) (term-type b))
  )
)
(defmethod equiv-terms ((a type-annotation) (b type-annotation))
  (and (equiv-terms (type-annotation-annotated a) 
               (type-annotation-annotated b))
       (eq (term-level a) (term-level b))
       (equiv-terms (term-type a) (term-type b))
  )
)
(defmethod equiv-terms ((a t) (b t))
  (equal a b)
)

;; ============
;; Mapping functions
;; ============

(declaim (ftype (function * list) mapappend-term))
(defgeneric mapappend-term (f obj succ-list &rest kvargs)
  (:documentation "Apply function F to each nodes of term OBJ one by one in a nesting way and append the results to SUCC-LIST.

F is a function which takes as arguments TERM 
and keyword arguments KVARGS and returns things of any types.

OBJ specifies a TERM.
OBJ may be tampered as side-effects of F.

KVARGS specifices keyword arguments of F.

Example: 
* (mappend-term F (func :argvari x :conseq a '(1 2 3))
(F x (F a '(1 2 3)) )")
)
(defmethod mapappend-term (f (obj atomic) succ-list &rest kvargs)
  (declare 
    (type (function (term list &key) term) f)
    (type list succ-list)
  )

  (match obj
    ( (structure atomic :type ty)
      (if ty (apply f ty succ-list kvargs))
    )
  )
)
(defmethod mapappend-term (f (obj func) succ-list &rest kvargs)
  (declare 
    (type (function (term list &key) term) f)
    (type list succ-list)
  )

  (match obj
    ( (structure func 
        :argvari argvari :conseq conseq :type ty
      )
      (apply f argvari 
        (apply f conseq 
          (if ty (apply f ty succ-list kvargs))
          kvargs
        )
        kvargs
      )
    )
  )
)
(defmethod mapappend-term (f (obj app) succ-list &rest kvargs)
  (declare 
    (type (function (term list &key) term) f)
    (type list succ-list)
  )

 (match obj
    ( (structure app :functor functor :arg arg :type ty)
      (apply f functor 
                (apply f arg
                        (if ty (apply f ty succ-list kvargs))
                        kvargs
                )
                kvargs
      )
    )
  )
)
(defmethod mapappend-term (f (obj type-annotation) succ-list &rest kvargs)
  (declare 
    (type (function (term list &key) term) f)
    (type list succ-list)
  )

  (match obj
    ( (structure type-annotation :annotated an :type ty)
      (apply f an 
            (if ty (apply f ty succ-list kvargs))
            kvargs
      )
    )
  )
)

(declaim (ftype (function * list) maplist-term))
(defgeneric maplist-term (f obj &rest kvargs)
  (:documentation "Apply function F to each nodes of term OBJ in a parallel way and append the results to SUCC-LIST.

F is a function which takes as arguments TERM 
and keyword arguments KVARGS and returns things of any types.

OBJ specifies a TERM.
OBJ may be tampered as side-effects of F.

KVARGS specifices keyword arguments of F.

Example: 
* (mappend-term F (func :argvari x :conseq a '(1 2 3))
(list (F x) (F a) '(1 2 3) )")
)
(defmethod maplist-term (f (obj atomic) &rest kvargs)
  (declare 
    (type (function (term list &key) term) f)
  )

  (match obj
    ( (structure atomic :type ty)

      (list (if ty (apply f ty kvargs)))
    )
  )
)
(defmethod maplist-term (f (obj func) &rest kvargs)
  (declare 
    (type (function (term list &key) term) f)
  )

  (match obj
    ( (structure func :argvari argvari :conseq conseq :type ty)
      (list 
        (apply f argvari kvargs)
        (apply f conseq kvargs)
        (if ty (apply f ty kvargs))
      )
    )
  )
)
(defmethod maplist-term (f (obj app) &rest kvargs)
  (declare 
    (type (function (term list &key) term) f)
  )

  (match obj
    ( (structure app :functor functor :arg arg :type ty)
      (list 
        (apply f functor kvargs)
        (apply f arg kvargs)
        (if ty (apply f ty kvargs))
      )
    )
  )
)
(defmethod maplist-term (f (obj type-annotation) &rest kvargs)
  (declare 
    (type (function (term list &key) term) f)
  )

  (match obj
    ( (structure type-annotation :annotated an :type ty)
      (list 
        (apply f an kvargs)
        (if ty (apply f ty kvargs))
      )
    )
  )
)

(declaim (ftype (function * term) map-term))
(defgeneric map-term (f obj &rest kvargs)
  (:documentation 
"Apply function F to each nodes of term OBJ to create a new term of the same type.

F is a function which takes as arguments TERM 
and keyword arguments KVARGS
and returns a new term of the same type.

OBJ specifies a TERM.
OBJ will be copied during the process and therefore be kept untouched.

KVARGS specifices keyword arguments of F.
")
)
(defmethod map-term (f (obj atomic) &rest kvargs)
  (declare
    (type (function (term list &key) term) f)
  )

  (match obj
    ( (structure atomic :type ty)
      (cond
        ( ty
          (let  ( (obj-copied (copy-structure obj)) )
            (setf (term-type obj-copied) (apply f ty kvargs))
            ;; return
            obj-copied
          )
        )
        ( t (copy-structure obj) )
      )
    )
  )
)
(defmethod map-term (f (obj func) &rest kvargs)
  (declare
    (type (function (term list &key) term) f)
  )

  (match obj
    ( (structure func :argvari argvari :conseq conseq :level level :type ty
        :constrs constrs
        :assignments assignments
        :free-vars free-vars
      )
      ;; return
      (make-func 
        :argvari      (apply f argvari kvargs)
        :conseq       (apply f conseq kvargs)
        :level        level
        :type         (and ty (apply f ty kvargs))
        :constrs      constrs
        :assignments  assignments
        :free-vars    free-vars
      )
    )
  )
)
(defmethod map-term (f (obj app) &rest kvargs)
  (declare
    (type (function (term list &key) term) f)
  )

  (match obj
    ( (structure app :functor functor :arg arg :level level :type ty
        :constrs      constrs
        :assignments  assignments
        :free-vars    free-vars
      )
      ;; return
      (make-app 
        :functor      (apply f functor kvargs)
        :arg          (apply f arg kvargs)
        :level        level
        :type         (and ty (apply f ty kvargs))
        :constrs      constrs
        :assignments  assignments
        :free-vars    free-vars
      )
    )
  )
)
(defmethod map-term (f (obj type-annotation) &rest kvargs)
  (declare
    (type (function (term list &key) term) f)
  )

  (match obj
    ( (structure type-annotation :annotated an :level level :type ty
      :constrs        constrs
        :assignments  assignments
        :free-vars    free-vars
      )
      ;; return
      (make-type-annotation 
        :annotated    (apply f an kvargs)
        :level        level
        :type         (and ty (apply f ty kvargs))
        :constrs      constrs
        :assignments  assignments
        :free-vars    free-vars
      )
    )
  )
)

;; TODO: DFS and BFS

(declaim (ftype (function (term) term) copyterm))
(defun copyterm (obj)
"Recursively copy term TERM."
  (declare  (type term obj))

  ;; map-term always make a copy of TERM.
  ;; So just make use of this property.
  (map-term #'identity obj)
)

(declaim (ftype (function (term) null) clear-terminfo))
(defun clear-terminfo (obj)
"Clear type-checking caches of TERM."
  (maplist-term #'clear-terminfo obj)
  (setf (term-constrs obj) nil)
  (setf (term-assignments obj) nil)
  (setf (term-free-vars obj) nil)

  ;; return
  nil
)