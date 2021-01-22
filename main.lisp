;;;; fist.lisp

(in-package #:fist)

(define-collision-groups
    (player-group 0)
    (enemy-group 1)
    (moon-group 2))

(gamekit:register-resource-package
 :keyword
 (asdf:system-relative-pathname :fist "resources/"))

(defgame game () ()
  (:viewport-width +width+)
  (:viewport-height +height+)
  (:viewport-title "fist"))

(gamekit:start 'game)

;;;
;; DEFINE SPRITE/SHEET ASSETS
(defvar *fist*
  (define-sprite "fist.png"
    :pos (vec/ (vec2 +width+ +height+) 2)
    :center (vec2 18 36)
    :bounding (vec2 37 72)
    :collide-with (list enemy-group moon-group)
    :collision-group player-group
    :collision t
    :render t))

(defvar *flame*
  (define-sprite "flame.png" :frames 2 :frame-length 30
    :frame-size (vec2 20 20) :center (vec2 10 10)
    :render nil :animate t))

(define-sprite "moon.png"
  :pos (vec2 (/ +width+ 2) +height+)
  :center (vec2 50 50)
  :bounding (vec2 100 100)
  :collide-with (list player-group)
  :collision-group moon-group
  :collision t
  :render t)

(gamekit:define-image :ufo1 "ufo1.png")
(gamekit:define-image :star1 "star1.png")

(defvar *ufo-pool*
  (create-spritepool (make-sprite :image :ufo1
                                  :current-frame 0
                                  :frames 2
                                  :frame-size (vec2 40 20)
                                  :frame-length 20
                                  :center (vec2 20 10)
                                  :animate t
                                  :bounding (vec2 40 20)
                                  :collision-group enemy-group
                                  :collide-with (list player-group)
                                  :collision t)
                     15))

(defvar *star1-pool*
  (create-spritepool (make-sprite :image :star1
                                  :current-frame 0
                                  :frames 2
                                  :frame-size (vec2 20 20)
                                  :frame-length 20
                                  :center (vec2 10 10)
                                  :animate t
                                  :bounding (vec2 20 20)
                                  :collision-group enemy-group
                                  :collide-with (list player-group)
                                  :collision t)
                     15))
;;;
;; BINDINGS
(gamekit:bind-button :right :pressed
                     (l (set-sprite-velocity *fist* :right *fist-speed*)))
(gamekit:bind-button :left :pressed
                     (l (set-sprite-velocity *fist* :left *fist-speed*)))
(gamekit:bind-button :up :pressed
                     (l (set-sprite-velocity *fist* :up *fist-speed*)))
(gamekit:bind-button :down :pressed
                     (l (set-sprite-velocity *fist* :down *fist-speed*)))

(gamekit:bind-button :space :pressed
                     (l (setf (sprite-render *flame*)
                              (not (sprite-render *flame*)))))

(gamekit:bind-button :s :pressed
                     (l (setf *spawn-enemies*
                              (not *spawn-enemies*))))


;; main draw function
(defmethod gamekit:draw ((app game))
  (gamekit:draw-rect +origin+ +width+ +height+ :fill-paint +black+)

  (loop for s in *sprites*
        when (sprite-render s) do
          (gamekit:draw s)))

;; main process function
(defmethod gamekit:act ((app game))
  (incf *current-tick*)
  ;; handle spritesheet animation
  (loop for s in *sprites*
        when (sprite-animate s) do
          (animate-sprite s))

  (process))
