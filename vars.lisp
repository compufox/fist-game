(in-package :fist)

;; constants
(defconstant +width+ 800)
(defconstant +height+ 600)
(define-constant +origin+ (vec2 0 0) :test #'vec=)
(define-constant +right+ (vec2 1 0) :test #'vec=)
(define-constant +left+ (vec2 -1 0) :test #'vec=)
(define-constant +up+ (vec2 0 1) :test #'vec=)
(define-constant +down+ (vec2 0 -1) :test #'vec=)
(defvar +black+ (vec4 0 0 0 1))

;; enemies 
(defparameter +max-enemies+ 10)
(defvar *enemy-count* 0)
(defparameter +enemy-spawn-delay+ 50)

(defvar *current-tick* 0)

;; sprite movement speeds
(defparameter *fist-speed* 5)

;; list sprite holder
(defvar *sprites* nil)
