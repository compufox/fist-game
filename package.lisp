;;;; package.lisp

(defpackage #:fist
  (:use #:cl)
  (:import-from :trivial-gamekit
                :defgame
                :vec2 :vec3 :vec4
                :x :y))