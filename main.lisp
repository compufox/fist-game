;;;; fist.lisp

(in-package #:fist)

(gamekit:register-resource-package
 :keyword
 (asdf:system-relative-pathname :fist "resources/"))

(defgame game () ()
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "fist"))

(gamekit:start 'game)

;;;
;; DEFINE SPRITE/SHEET ASSETS
(defvar *fist*
  (define-sprite "fist.png"
    :pos (vec/ (vec2 *width* *height*) 2)
    :center (vec2 18 36)
    :render t))

(defvar *flame*
  (define-sprite "flame.png" :frames 2 :frame-length 30
    :frame-size (vec2 20 20) :center (vec2 10 10)
    :render nil :animate t))

(define-sprite "moon.png"
  :pos (vec2 (/ *width* 2) *height*)
  :center (vec2 50 50)
  :render t)


;;;
;; BINDINGS
(gamekit:bind-button :right :pressed
                     (l (move-fist :right)))
(gamekit:bind-button :left :pressed
                     (l (move-fist :left)))

(gamekit:bind-button :space :pressed
                     (l (setf (sprite-render *flame*)
                              (not (sprite-render *flame*)))))

(gamekit:bind-button :right :repeating
                     (l (move-fist :right)))
(gamekit:bind-button :left :repeating
                     (l (move-fist :left)))



;; main draw function
(defmethod gamekit:draw ((app game))
  (gamekit:draw-rect *origin* *width* *height* :fill-paint *black*)

  (loop for s in *sprites*
        when (sprite-render s) do
          (gamekit:draw s)))

;; main process function
(defmethod gamekit:act ((app game))
  ;; handle spritesheet animation
  (loop for s in *sprites*
        when (sprite-animate s) do
          (animate-sprite s))

  (process))
