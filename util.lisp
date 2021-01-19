(in-package :fist)

(defstruct sprite
  image pos size center render)

(defstruct spritesheet
  image current-frame frames sprite-size
  frame-length pos center tick render animate)

(defvar *sprites* nil)
(defvar *spritesheets* nil)

(defun animate-spritesheet (sheet)
  (let ((tick (spritesheet-tick sheet))
        (length (spritesheet-frame-length sheet))
        (frame (spritesheet-current-frame sheet))
        (max-frames (spritesheet-frames sheet)))
    (setf (spritesheet-tick sheet)
          (mod (1+ tick) length))

    (when (zerop (spritesheet-tick sheet))
      (setf (spritesheet-current-frame sheet)
            (mod (1+ frame) max-frames)))))

;; TODO: substitute _ for -
(defun to-keyword (thing)
  "convert THING to keyword"
  (intern (string-upcase thing)
          :keyword))

(defmethod vec- ((v1 vec2) (v2 vec2))
  (vec2 (- (x v1) (x v2))
        (- (y v1) (y v2))))

(defmethod vec- ((v1 vec2) (n number))
  (vec- v1 (vec2 n n)))

(defmethod vec/ ((v1 vec2) (v2 vec2))
  (vec2 (/ (x v1) (x v2))
        (/ (y v1) (y v2))))

(defmethod vec/ ((v1 vec2) (n number))
  (vec/ v1 (vec2 n n)))

(defmethod vec* ((v1 vec2) (v2 vec2))
  (vec2 (* (x v1) (x v2))
        (* (y v1) (y v2))))

(defmethod vec* ((v1 vec2) (n number))
  (vec* v1 (vec2 n n)))

(defun move-fist (dir)
  "move the fist in a direction"
  (let ((pos (sprite-pos *fist*)))
    (setf (sprite-pos *fist*)
          (case dir
            (:right (vec2 (min *width* (+ (x pos) *speed*))
                          (y pos)))
            (:left (vec2 (max 0 (- (x pos) *speed*))
                          (y pos)))))))

(defmacro l (&body body)
  "shorthand way to write lambdas. accepts two arguments _ and __"
  `(lambda (&optional _ __)
     (declare (ignorable _ __))
     ,@body))

(defmacro define-sprite (path &key pos center name render)
  "defines a sprite at PATH, settings is POSition, CENTER, and an optional NAME

POS defaults to (vec2 0 0)
CENTER defaults to (vec2 0 0)
NAME is used for gamekit internal image name. if not provided it is generated from image file name
RENDER, if non-nil, sprite will be rendered."
  (let ((sprite-name (or name (to-keyword (pathname-name path)))))
    `(let ((sprite (make-sprite :image ,sprite-name
                                :render ,render
                                :pos (or ,pos (vec2 0 0))
                                :center (or ,center (vec2 0 0)))))
       (gamekit:define-image ,sprite-name ,path)
       ;; a sleep here is the only way ive found that doesnt make
       ;;  image-width/height error out trying to find the image 
       ;;  defined by sprite-name
       (sleep 0.1)
       (setf (sprite-size sprite)
             (vec2 (gamekit:image-width ,sprite-name)
                   (gamekit:image-height ,sprite-name)))
       (push sprite *sprites*)
       sprite)))
       

(defmacro define-spritesheet (path &key name frames frame-length sprite-size pos center render animate)
  "defines a spritesheet at PATH, including how many FRAMES, how long each frame should last, how big each sprite is, the sprite's POSition, and CENTER.
expects a spritesheet that has all sprites aligned along the X axis, and not aligned along the Y axis

POS and CENTER default to (vec2 0 0)
NAME is used for gamekit internal image name. if not provided it is generated from image file name
SPRITE-SIZE *MUST* be provided.
FRAMES is determined automatically if not provided.
RENDER is nil by default. if set to non-nil then the spritesheet will be rendered
ANIMATE is nil by default. if set to non-nil then the spritesheet will animate"
  (let ((sheet-name (or name (to-keyword (pathname-name path)))))
    `(let ((sheet (make-spritesheet :image ,sheet-name
                                    :current-frame 0
                                    :tick 0
                                    :animate ,animate
                                    :render ,render
                                    :sprite-size ,sprite-size
                                    :pos (or ,pos (vec2 0 0))
                                    :center (or ,center (vec2 0 0))
                                    :frame-length ,frame-length)))
       (gamekit:define-image ,sheet-name ,path)
       (sleep 0.1)
       (setf (spritesheet-frames sheet)
             (or ,frames (/ (gamekit:image-width ,sheet-name)
                            (x ,sprite-size))))
       (push sheet *spritesheets*)
       sheet)))
