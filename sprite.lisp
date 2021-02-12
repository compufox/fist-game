(in-package :fist)

;; define SPRITE struct
(defstruct sprite
  (tick 0)
  (current-frame 0)
  (vel +origin+)
  (rotation 0)
  (scale (vec2 1 1))
  image frames frame-size animate
  frame-length pos center render 
  bounding collision-group collide-with
  collision)

;; define sprite pool struct
(defstruct spritepool
  sprite amount pool)

(defun animate-sprite (sprite)
  "helper function that animates a SPRITE every frame"
  (when sprite
    (let ((tick (sprite-tick sprite))
          (length (sprite-frame-length sprite))
          (frame (sprite-current-frame sprite))
          (max-frames (sprite-frames sprite)))
      (setf (sprite-tick sprite)
            (mod (1+ tick) length))
      
      (when (zerop (sprite-tick sprite))
        (setf (sprite-current-frame sprite)
              (mod (1+ frame) max-frames))))))

(defun create-spritepool (sprite amount)
  "creates a spritepool containing AMOUNT of sprites, using SPRITE as a template"
  (let ((p (make-spritepool :sprite sprite :amount amount :pool nil)))
    (dotimes (i amount)
      (let ((s (make-sprite :image (sprite-image sprite)
                            :frames (sprite-frames sprite)
                            :frame-size (sprite-frame-size sprite)
                            :frame-length (sprite-frame-length sprite)
                            :bounding (sprite-bounding sprite)
                            :collision (sprite-collision sprite)
                            :collision-group (sprite-collision-group sprite)
                            :collide-with (sprite-collide-with sprite)
                            :pos (sprite-pos sprite)
                            :scale (sprite-scale sprite)
                            :rotation (sprite-rotation sprite)
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

(defun sprite-center-pos (s)
  (let ((pos (sprite-pos s))
        (center (sprite-center s)))
    (vec+ pos center)))

(defmacro define-sprite (sprite-name &key (frames 0) (frame-length 0) frame-size pos center bounding collision-group collide-with render animate collision scale rotation)
  "defines a sprite at PATH, including how many FRAMES, how long each frame should last, how big each sprite is, the sprite's POSition, and CENTER.
expects a spritesheet that has all sprites aligned along the X axis, and none aligned along the Y axis

POS and CENTER default to *origin*
NAME is used for gamekit internal image name. if not provided it is generated from image file name
SPRITE-SIZE *MUST* be provided.
FRAMES is determined automatically if not provided.
RENDER is nil by default. if set to non-nil then the spritesheet will be rendered
ANIMATE is nil by default. if set to non-nil then the spritesheet will animate"
  `(let ((s (make-sprite :image ,sprite-name
                         :animate ,animate
                         :render ,render
                         :frames ,frames
                         :bounding (or ,bounding ,frame-size +origin+)
                         :collision ,collision
                         :scale (or ,scale (vec2 1 1))
                         :rotation (or ,rotation 0)
                         :collision-group ,collision-group
                         :collide-with ,collide-with
                         :frame-size (or ,frame-size +origin+)
                         :pos (or ,pos +origin+)
                         :center (or ,center +origin+)
                         :frame-length ,frame-length)))
     (push s *sprites*)
     s))

;; defines how to draw a sprite on the screen
(defmethod gamekit:draw ((s sprite))
  (let ((frame (sprite-current-frame s))
        (size (sprite-frame-size s))
        (pos (sprite-pos s))
        (center (sprite-center s))
        (scale (sprite-scale s))
        (rot (sprite-rotation s)))
    (gamekit:scale-canvas (x scale) (y scale))
    (gamekit:rotate-canvas (/ (* rot 3.14159) 180))
    (gamekit:draw-image (vec- pos center)
                        (sprite-image s)
                        :origin (vec2 (* frame (x size)) 0)
                        :width (x size)
                        :height (y size))))

(defun set-sprite-menu-position ()
  (when (and *moon* *fist* *flame*)
    (setf (sprite-pos *moon*) +moon-menu-pos+
          (sprite-scale *moon*) (vec2 2 2)
          
          (sprite-pos *fist*) (vec2 (x +fist-menu-pos+) (y +fist-menu-pos+))
          (sprite-scale *fist*) (vec2 2 2)
          
          (sprite-pos *flame*) (vec- (vec2 (+ (x +fist-menu-pos+) 5)
                                           (- (y +fist-menu-pos+)
                                              (y (sprite-center *fist*))))
                                     (sprite-center *flame*))
          (sprite-scale *flame*) (vec2 2 2))))
