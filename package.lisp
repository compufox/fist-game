;;;; package.lisp

(defpackage #:fist
  (:use #:cl #:gamekit.timeline #:gamekit.simple-menus)
  (:import-from :alexandria
                :define-constant)
  (:import-from :trivial-gamekit
                :defgame
                :vec2 :vec3 :vec4
                :x :y))
