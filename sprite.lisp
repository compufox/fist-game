(in-package :fist)

;; define SPRITE struct
(defstruct sprite
  image current-frame frames frame-size
  frame-length pos center tick render animate)

;; define sprite pool struct
(defstruct spritepool
  sprite amount pool)

(defun animate-sprite (sprite)
  "helper function that animates a SPRITE every frame"
  (let ((tick (sprite-tick sprite))
        (length (sprite-frame-length sprite))
        (frame (sprite-current-frame sprite))
        (max-frames (sprite-frames sprite)))
    (setf (sprite-tick sprite)
          (mod (1+ tick) length))

    (when (zerop (sprite-tick sprite))
      (setf (sprite-current-frame sprite)
            (mod (1+ frame) max-frames)))))

(defun create-spritepool (sprite amount)
  "creates a spritepool containing AMOUNT of sprites, using SPRITE as a template"
  (let ((p (make-spritepool :sprite sprite :amount amount :pool nil)))
    (dotimes (i amount)
      (let ((s (make-sprite :image (sprite-image sprite)
                            :current-frame 0
                            :tick 0
                            :frames (sprite-frames sprite)
                            :frame-size (sprite-frame-size sprite)
                            :frame-length (sprite-frame-length sprite)
                            :pos (sprite-pos sprite)
                            :center (sprite-center sprite)
                            :render (sprite-render sprite)
                            :animate (sprite-animate sprite))))
        (push s (spritepool-pool p))
        (push s *sprites*)))
    p))

(defun spritepool-get-next (sp)
  "return the next non-rendering sprite from the pool"
  (loop for s in (spritepool-pool sp)
        unless (sprite-render s)
          return s))

(defmacro define-sprite (path &key name (frames 0) (frame-length 0) frame-size pos center render animate)
  "defines a sprite at PATH, including how many FRAMES, how long each frame should last, how big each sprite is, the sprite's POSition, and CENTER.
expects a spritesheet that has all sprites aligned along the X axis, and none aligned along the Y axis

POS and CENTER default to *origin*
NAME is used for gamekit internal image name. if not provided it is generated from image file name
SPRITE-SIZE *MUST* be provided.
FRAMES is determined automatically if not provided.
RENDER is nil by default. if set to non-nil then the spritesheet will be rendered
ANIMATE is nil by default. if set to non-nil then the spritesheet will animate"
  (let ((sprite-name (or name (to-keyword (pathname-name path)))))
    `(let ((s (make-sprite :image ,sprite-name
                           :current-frame 0
                           :tick 0
                           :animate ,animate
                           :render ,render
                           :frames ,frames
                           :frame-size (or ,frame-size +origin+)
                           :pos (or ,pos +origin+)
                           :center (or ,center +origin+)
                           :frame-length ,frame-length)))
       (gamekit:define-image ,sprite-name ,path)
       (sleep 0.1)
       (unless ,frame-size
         (setf (sprite-frame-size s)
               (vec2 (gamekit:image-width ,sprite-name)
                     (gamekit:image-height ,sprite-name))))
       (push s *sprites*)
       s)))

;; defines how to draw a sprite on the screen
(defmethod gamekit:draw ((s sprite))
  (let ((frame (sprite-current-frame s))
        (size (sprite-frame-size s))
        (pos (sprite-pos s))
        (center (sprite-center s)))
    (gamekit:draw-image (vec- pos center)
                        (sprite-image s)
                        :origin (vec2 (* frame (x size)) 0)
                        :width (x size)
                        :height (y size))))
