(in-package :fist)

(defstruct sprite
  image pos size center)

(defstruct spritesheet
  image current-frame frames sprite-size
  frame-length pos center tick)

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

(defmacro define-sprite (path &key pos center name)
  (let ((sprite-name (or name (to-keyword (pathname-name path)))))
    `(progn
       (gamekit:define-image ,sprite-name ,path)
       (sleep 0.1)
       (make-sprite :image ,sprite-name
                    :pos (or ,pos (vec2 0 0))
                    :size (vec2 (gamekit:image-width ,sprite-name)
                                (gamekit:image-height ,sprite-name))
                    :center (or ,center (vec2 0 0))))))

(defmacro define-spritesheet (path &key name frames frame-length sprite-size pos center)
  (let ((sheet-name (or name (to-keyword (pathname-name path)))))
    `(progn
       (gamekit:define-image ,sheet-name ,path)
       (sleep 0.1)
       (make-spritesheet :image ,sheet-name
                         :current-frame 0
                         :tick 0
                         :sprite-size ,sprite-size
                         :frames ,frames
                         :pos (or ,pos (vec2 0 0))
                         :center (or ,center (vec2 0 0))
                         :frame-length ,frame-length))))
