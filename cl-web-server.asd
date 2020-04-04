;;;; cl-web-server.asd

(asdf:defsystem #:cl-web-server
  :description "Describe cl-web-server here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot
               #:uiop)
  :components ((:file "package")
               (:file "server")))
