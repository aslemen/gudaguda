(in-package :gudaguda)

;; ============
;; Data structure declarations
;; ============

(defstruct term
  (level            0   :type (integer 0))
  (type             nil :type (or null term))
  (constrs          nil :type list)
  (constrs-local    nil :type list)
  (assignments      nil :type list)
  (free-vars        nil :type list)
)

(defstruct (atomic        (:include term))
  (data nil :type t)
)

(defstruct (const         (:include atomic)))
(defstruct (vari          (:include atomic)))
(defstruct (vari-typevari (:include vari)))

(defstruct (func (:include term))
  (argvari nil :type vari)
  (conseq  nil :type term)
)

(defstruct (app (:include term))
  (functor nil :type term)
  (arg     nil :type term)
)

(defstruct (type-annotation (:include term))
  (annotated nil :type term)
)

;; ============
;; Mapping functions
;; ============

(declaim (ftype (function * list) mapappend-term))
(defgeneric mapappend-term (f obj succ-list &rest kvargs)
  (:documentation "TBW")
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
  (:documentation "TBW")
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
        (apply #'maplist-term f an kvargs)
        (if ty (apply #'maplist-term f ty kvargs))
      )
    )
  )
)

(declaim (ftype (function * term) map-term))
(defgeneric map-term (f obj &rest kvargs)
  (:documentation 
"A recursion helper to apply a function to a term recursively.
Structures will kept untouched and be copied.

F is a function from TERM (and keyword arguments) to TERM.
OBJ specifies a TERM.
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
  (declare  (type term obj))

  ;; map-term always make a copy of TERM.
  ;; So just make use of this property.
  (map-term #'identity obj)
)

(declaim (ftype (function (term) null) clear-terminfo))
(defun clear-terminfo (obj)
  (maplist-term #'clear-terminfo obj)
  (setf (term-constrs obj) nil)
  (setf (term-assignments obj) nil)
  (setf (term-free-vars obj) nil)

  ;; return
  nil
)