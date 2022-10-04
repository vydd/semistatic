(defsystem "semistatic"
  :version "0.1.0"
  :author "vydd"
  :license "MIT"
  :depends-on ("lquery"
               "hunchentoot"
               "easy-routes"
               "lisp-magick-wand"
               "cl-date-time-parser"
               "spinneret"
               "cl-slug"
               "local-time")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "semistatic/tests"))))

(defsystem "semistatic/tests"
  :author "vydd"
  :license "MIT"
  :depends-on ("semistatic"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for semistatic"
  :perform (test-op (op c) (symbol-call :rove :run c)))
