;;;; fist.lisp

(in-package #:fist)

(defvar *width* 800)
(defvar *height* 600)

(defvar *origin* (vec2 0 0))
(defvar *black* (vec4 0 0 0 1))

(defvar *boosting* nil)
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
                 :pos (vec2 (/ *width* 2)
                            (/ *height* 2))
                 :center (vec2 18 36)))

(defvar *flame1*
  (define-sprite "flame1.png"))

(defvar *flame2*
  (define-sprite "flame2.png"))

(defvar *moon*
  (define-sprite "moon.png"
                 :pos (vec2 (/ *width* 2) *height*)
                 :center (vec2 50 50)))

(defmethod gamekit:draw ((s sprite))
  (let ((size (sprite-size s))
        (pos (sprite-pos s))
        (center (sprite-center s)))
    (gamekit:draw-image (vec2 (- (x pos) (x center))
                              (- (y pos) (y center)))
                        (sprite-image s)
                        :width (x size)
                        :height (y size))))

(defmethod gamekit:act ((app game))
  (setf (sprite-pos *flame1*)
        (vec- (sprite-pos *fist*)
              (vec2 13 56))))

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
  (gamekit:draw *fist*)
  (gamekit:Draw *moon*)
  (when *boosting*
    (gamekit:Draw *flame1*)))
