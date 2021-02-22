;;;; fist.lisp

(in-package #:fist)

(define-collision-groups
    (player-group 0)
    (enemy-group 1)
    (moon-group 2))

(gamekit:register-resource-package :keyword
 (asdf:system-relative-pathname :fist "resources/"))

(defgame game (gamekit.fistmachine:fistmachine) ()
  (:default-initargs :initial-state 'menu-state)
  (:viewport-width +width+)
  (:viewport-height +height+)
  (:viewport-title "fist"))

(defmethod gamekit:notice-resources ((this game) &rest resource-names)
  (gamekit:play-sound :music :looped-p t))

;;;
;; DEFINE FONTS
(gamekit:define-font :krona "KronaOne-Regular.ttf")

;;;
;; DEFINE SOUNDS
(gamekit:define-sound :music "spaceship.wav")
(gamekit:define-sound :explosion1 "explosion1.wav")
(gamekit:define-sound :explosion2 "explosion2.wav")
(gamekit:define-sound :blastoff "blastoff.wav")

;;;
;; DEFINE SPRITES/IMAGES
(gamekit:define-image :menu-bg "start_screen.png")
(gamekit:define-image :fist "fist.png")
(gamekit:define-image :moon "moon.png")
(gamekit:define-image :flame "flame.png")
(gamekit:define-image :ufo1 "ufo1.png")
(gamekit:define-image :star1 "star1.png")
(gamekit:define-image :game-bg "game_bg.png")

(setf
 *fist* (define-sprite :fist
          :pos (vec/ (vec2 +width+ +height+) 2)
          :center (vec2 18 36)
          :bounding (vec2 37 72)
          :frame-size (vec2 37 72)
          :collide-with (list enemy-group moon-group)
          :collision-group player-group
          :collision t
          :render t)
 *flame* (define-sprite :flame
           :frames 2
           :frame-length 30
           :frame-size (vec2 20 20)
           :center (vec2 10 10)
           :render nil :animate t)
 *moon* (define-sprite :moon
          :pos (vec2 (/ +width+ 2) +height+)
          :center (vec2 50 50)
          :bounding (vec2 100 100)
          :frame-size (vec2 100 100)
          :collide-with (list player-group)
          :collision-group moon-group
          :render nil
          :collision t)
 *ufo-pool*
 (create-spritepool (make-instance 'sprite :image :ufo1
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
                    15)
 *star1-pool*
 (create-spritepool (make-instance 'sprite :image :star1
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

(define-sprite :game-bg
      :frames 2
      :pos +origin+
      :frame-size (vec2 800 600)
      :frame-length 120
      :animate t
      :render t)

;; define keyframes for our start screen sprites
(make-keyframe *start-timeline* 0
               :event (l (uninitialize-menu *start-menu*)
                         (setf (sprite-render *flame*) t)
                         (gamekit:play-sound :blastoff)))
(make-keyframe *start-timeline* 120
               :object *fist* :slot 'pos
               :target (vec2 280 350))
(make-keyframe *start-timeline* 120
               :object *flame* :slot 'pos
               :target (vec2 275 304)
               :event (l (gamekit.fistmachine:transition-to 'playing-state)))

;; our callback for our start menu
(defun menu-callback (selected)
  (case selected
    (:start (progn
              (restart-timeline *start-timeline*)
              (play-timeline *start-timeline*)))
    (:quit (gamekit:stop))))

;; game state functions
(defmethod gamekit:draw ((this menu-state))
  (gamekit:draw-image +origin+ :menu-bg)
  (when (sprite-render *flame*)
    (gamekit:with-pushed-canvas ()
      (gamekit:draw *flame*)))
  (gamekit:with-pushed-canvas ()
    (gamekit:draw *fist*))
  (gamekit:with-pushed-canvas ()
    (gamekit:draw *moon*))

  (when (and *start-menu* *small-font*)
    (draw-text "music by mrpoly" (vec2 10 10)
               :color +white+ :font *small-font*
               :center nil)
    (draw-menu *start-menu*)))
                      
(defmethod gamekit:act ((this menu-state))
  ;; ensure sprite size and location on menu screen
  (animate-sprite *flame*))

(defmethod gamekit:draw ((this playing-state))

  ;; after we have "traveled" far enough, render the moon
  (when (< (+ (y (sprite-pos *moon*)) (gamekit:image-height :moon))
           0)
    (setf (failed this) t))
  
  (loop for s in *sprites*
        when (sprite-render s) do
          ;; when rotates/scales it changes the position on the main canvas.
          ;; determine how to fix that.
          (gamekit:with-pushed-canvas () (gamekit:draw s)))

  (when (paused this)
    (draw-text "P A U S E D" (vec/ (vec2 +width+ +height+) 2)
               :color +white+ :font *large-font*))
  
  ;; when we've been hit or we missed the moon, the game is over, so we say as much
  (when (failed this)
    (draw-text "MISSION FAILED :c" (vec/ (vec2 +width+ +height+) 2)
               :font *large-font* :color +white+))
  (when (succeeded this)
    (draw-text "MOON STATUS: PUNCHED" (vec/ (vec2 +width+ +height+) 2)
               :font *large-font* :color +white+)))

;; main process function
(defmethod gamekit:act ((this playing-state))
  (unless (paused this)
    (incf (tick this))
    ;; handle spritesheet animation
    (loop for s in *sprites*
          when (sprite-animate s) do
            (animate-sprite s))

    (unless (or (failed this)
                (succeeded this))
      (process this))))
