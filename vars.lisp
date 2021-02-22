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
(defvar +white+ (vec4 1 1 1 1))
(define-constant +fist-menu-pos+ (vec2 280 110) :test #'vec=)
(define-constant +moon-menu-pos+ (vec2 90 240) :test #'vec=)

;; enemies 
(defparameter +max-enemies+ 10)
(defvar *enemy-count* 0)
(defparameter +enemy-spawn-delay+ 50)

;; sprite movement speeds
(defparameter *fist-speed* 5)
(defparameter *max-distance* 6000)

;; list sprite holder
(defvar *sprites* nil)

;; special sprite vars
(defvar *moon* nil)
(defvar *fist* nil)
(defvar *flame* nil)
(defvar *ufo-pool* nil)
(defvar *star1-pool* nil)
(defvar *large-font* nil)
(defvar *small-font* nil)
(defvar *game-bg* nil)
(defvar *start-menu* nil)
(defvar *menu-timeline* (make-timeline))
(defvar *start-timeline* (make-timeline))
