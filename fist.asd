;;;; fist.asd

(asdf:defsystem #:fist
  :description "Describe fist here"
  :author "ava fox"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:trivial-gamekit #:trivial-gamekit-fistmachine
               #:trivial-gamekit-timeline #:trivial-gamekit-simple-menus)
  :components ((:file "package")
               (:file "util")
               (:file "vars")
               (:file "states")
               (:file "sprite")
               (:file "logic")
               (:file "main")))
