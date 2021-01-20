(in-package :fist)

;; screen width/height
(defvar *width* 800)
(defvar *height* 600)

(defvar *origin* (vec2 0 0))
(defvar *black* (vec4 0 0 0 1))

;; speed of sprite movement
(defparameter *speed* 20)

;; list sprite holder
(defvar *sprites* nil)
