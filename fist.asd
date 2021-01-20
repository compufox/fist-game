;;;; fist.asd

(asdf:defsystem #:fist
  :description "Describe fist here"
  :author "ava fox"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:trivial-gamekit)
  :components ((:file "package")
               (:file "vars")
               (:file "util")
               (:file "sprite")
               (:file "logic")
               (:file "main")))
