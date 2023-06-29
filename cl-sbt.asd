(defsystem "cl-sbt"
  :version "0.1.0"
  :author "Marcus Kammer"
  :license ""
  :depends-on (:spinneret)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-sbt/tests"))))

(defsystem "cl-sbt/tests"
  :author "Marcus Kammer"
  :license ""
  :depends-on ("cl-sbt"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-sbt"
  :perform (test-op (op c) (symbol-call :rove :run c)))
