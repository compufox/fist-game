;;;; fist.lisp

(in-package #:fist)

(defvar *width* 800)
(defvar *height* 600)

(defvar *origin* (vec2 0 0))
(defvar *black* (vec4 0 0 0 1))

(defparameter *speed* 20)

(gamekit:register-resource-package
 :keyword
 (asdf:system-relative-pathname :fist "resources/"))

(defgame game () ()
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "fist"))

(gamekit:start 'game)

(defvar *fist*
  (define-sprite "fist.png"
    :pos (vec/ (vec2 *width* *height*) 2)
    :center (vec2 18 36)
    :render t))

(defvar *flame*
  (define-spritesheet "flame.png" :frames 2 :frame-length 30
    :sprite-size (vec2 20 20) :center (vec2 10 10)
    :render nil :animate t))

(define-sprite "moon.png"
  :pos (vec2 (/ *width* 2) *height*)
  :center (vec2 50 50)
  :render t)

(defmethod gamekit:draw ((s sprite))
  (let ((size (sprite-size s))
        (pos (sprite-pos s))
        (center (sprite-center s)))
    (gamekit:draw-image (vec- pos center)
                        (sprite-image s)
                        :width (x size)
                        :height (y size))))

(defmethod gamekit:draw ((s spritesheet))
  (let ((frame (spritesheet-current-frame s))
        (size (spritesheet-sprite-size s))
        (pos (spritesheet-pos s))
        (center (spritesheet-center s)))
    (gamekit:draw-image (vec- pos center)
                        (spritesheet-image s)
                        :origin (vec2 (* frame (x size)) 0)
                        :width (x size)
                        :height (y size))))

(gamekit:bind-button :right :pressed
                     (l (move-fist :right)))
(gamekit:bind-button :left :pressed
                     (l (move-fist :left)))

(gamekit:bind-button :right :repeating
                     (l (move-fist :right)))
(gamekit:bind-button :left :repeating
                     (l (move-fist :left)))

(defmethod gamekit:draw ((app game))
  (gamekit:draw-rect *origin* *width* *height* :fill-paint *black*)

  (loop for s in *sprites*
        when (sprite-render s) do
          (gamekit:draw s))
  (loop for s in *spritesheets*
        when (spritesheet-render s) do
          (gamekit:draw s)))

(defmethod gamekit:act ((app game))
  ;; handle spritesheet animation
  (loop for s in *spritesheets*
        when (spritesheet-animate s) do
          (animate-spritesheet s))
  (setf (spritesheet-pos *flame*)
        (vec- (sprite-pos *fist*)
              (vec2 5 46))))
