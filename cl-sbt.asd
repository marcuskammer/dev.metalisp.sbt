(defsystem "cl-sbt"
  :version "0.1.0"
  :author "Marcus Kammer"
  :license "MIT"
  :depends-on (:spinneret)
  :components ((:module "src"
                :components
                        ((:file "main")
                         (:file "page")))
               (:module "src/component"
                :components
                        ((:file "accordion")
                         (:file "alert")
                         (:file "badge")
                         (:file "button")
                         (:file "card")
                         (:file "dropdown")
                         (:file "header")
                         (:file "list-group")
                         (:file "navbar")
                         (:file "nav-tab")
                         (:file "pagination")
                         (:file "table"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-sbt/tests"))))

(defsystem "cl-sbt/examples"
  :author "Marcus Kammer"
  :license "MIT"
  :depends-on ("cl-sbt")
  :components ((:module "examples/album"
                :components
                        ((:file "index")
                         (:file "_header")
                         (:file "_main")
                         (:file "_footer")
                         (:file "_navbar")
                         (:file "_card")))))

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
