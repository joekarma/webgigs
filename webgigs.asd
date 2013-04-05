;;;; webgigs.asd

(asdf:defsystem #:webgigs
  :serial t
  :description "Webgigs is a client for viewing and responding to craigslist ads for web developers."
  :author "Joe Taylor <taylor@c0de.co>"
  :license "BSD Style"
  :components ((:file "package")
               (:file "core")
               (:file "db")
               (:file "webgigs"))
  :depends-on (:restas :closure-html :stp-query :drakma :cl-ppcre :cl-interpol :postmodern :css-lite :parenscript :yaclml
               :alexandria :config))

