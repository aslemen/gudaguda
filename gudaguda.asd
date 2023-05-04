(defsystem "gudaguda"
  :description "An unsophisticated implementation of dependently typed "
  :version "0.0.0.0"
  :author "Nori Hayashi <net@hayashi-lin.net>"
  :license "to be determined"
  :depends-on (
    :iterate
    :trivia
    :queues.simple-queue
  )
  :serial t
  :components (
    (:file "base")
    (:file "obj")
    (:file "reduce")
    (:file "infer")
    (:file "io")
  )
  :in-order-to (
    (test-op (test-op gudaguda/test))
  )
)

(defsystem "gudaguda/test"
  :depends-on (
    :iterate
    :fiveam
    :gudaguda
  )
  :serial t
  :components (
    (:module "test"
        :components (
            (:file "main")
            ;; component tests
            (:file "io")
            ;; intergration tests
        )
    )
  )
  :perform (
    test-op (o s)
      (symbol-call :fiveam :run! :gudaguda)
  )
)
