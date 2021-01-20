(in-package :fist)

;; constants
(defconstant +width+ 800)
(defconstant +height+ 600)
(defconstant +origin+ (vec2 0 0))
(defconstant +black+ (vec4 0 0 0 1))

;; speed of sprite movement
(defparameter *speed* 20)

;; list sprite holder
(defvar *sprites* nil)
