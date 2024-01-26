(defsystem "dev.metalisp.sbt"
  :version "0.1.0"
  :author "Marcus Kammer <marcus.kammer@metalisp.dev"
  :license "GNU Lesser General Public License"
  :source-control (:git "git@git.sr.ht:~marcuskammer/dev.metalisp.sbt")
  :depends-on (:spinneret :dexador)
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "translate")
                 (:file "utility")))
               (:module "src/layout"
                :depends-on ("src")
                :components
                ((:file "grid")))
               (:module "src/component"
                ;; Components are the building blocks for Patterns
                :depends-on ("src")
                :components
                ((:file "accordion")
                 (:file "alert")
                 (:file "badge")
                 (:file "button")
                 (:file "card")
                 (:file "dropdown")
                 (:file "list-group")
                 (:file "navbar")
                 (:file "nav-tab")
                 (:file "pagination")
                 (:file "table")
                 (:file "spinner")
                 (:file "form")))
               (:module "src/pattern"
                ;; Patterns are built on top of components and define their own DSL to create specific things
                :depends-on ("src" "src/component")
                :components
                ((:file "questionnaire")))
               (:module "examples"
                ;; Examples show how different patterns work together
                :depends-on ("src/pattern")
                :components
                ((:file "album"))))
  :description "A Common Lisp library for generating Bootstrap-based HTML markup. It provides macros to easily create Bootstrap components such as accordions, alerts, badges, buttons, cards, dropdowns, headers, list groups, navbars, nav-tabs, pagination, and tables. This library is dependent on the Spinneret library for HTML generation."
  :in-order-to ((test-op (test-op "dev.metalisp.sbt/tests"))))

(defsystem "dev.metalisp.sbt/tests"
  :author "Marcus Kammer"
  :license "MIT"
  :depends-on ("dev.metalisp.sbt"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main")
                 (:file "utility")))
               (:module "tests/layout"
                :components
                ((:file "grid")))
               (:module "tests/component"
                :components
                ((:file "accordion")
                 (:file "alert")
                 (:file "badge")
                 (:file "button")
                 (:file "card")
                 (:file "dropdown")
                 (:file "list-group")
                 (:file "navbar")
                 (:file "nav-tab")
                 (:file "form")))
               (:module "tests/pattern"
                :components
                ((:file "questionnaire"))))
  :description "Test system for dev.metalisp.sbt"
  :perform (test-op (op c) (symbol-call :rove :run c)))
