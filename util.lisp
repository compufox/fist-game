(in-package :fist)

(declaim (inline to-keyword random-side))

;; TODO: substitute _ for -
(defun to-keyword (thing)
  "convert THING to keyword"
  (intern (string-upcase thing)
          :keyword))

(defun vecx+ (v1 v2)
  (+ (x v1) (x v2)))

(defun vecx- (v1 v2)
  (- (x v1) (x v2)))

(defun vecy+ (v1 v2)
  (+ (y v1) (y v2)))

(defun vecy- (v1 v2)
  (- (y v1) (y v2)))

(defmethod vec= ((v1 vec2) (v2 vec2))
  (and (= (x v1) (x v2))
       (= (y v1) (y v2))))

(defmethod vec+ ((v1 vec2) (v2 vec2))
  (vec2 (+ (x v1) (x v2))
        (+ (y v1) (y v2))))

(defmethod vec+ ((v1 vec2) (n number))
  (vec+ v1 (vec2 n n)))

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

(defmacro l (&body body)
  "shorthand way to write lambdas. accepts two arguments _ and __"
  `(lambda (&optional _ __)
     (declare (ignorable _ __))
     ,@body))

(defun get-random-from-pool (&rest pools)
  "returns a random sprite from one of the POOLS"
  (loop with chance = 0.4

        for (sprite . perc) in (loop for p in pools
                                     collect (cons (spritepool-get-next p)
                                                   (random 1.0)))
        when (<= perc chance)
          return sprite))

(defun random-side ()
  (if (> (random 1.0) .5)
      +width+
      0))

(defmacro define-collision-groups (&rest col-pairs)
  `(progn
     ,@(loop for p in col-pairs
             collect `(defvar ,(car p) ,(cadr p)))))

(defun sprites-overlap-p (s1 s2)
  (let* ((bb1 (sprite-bounding s1))
         (pos1 (sprite-pos s1))

         (sprite1-bottom (y pos1))
         (sprite1-top (vecy+ pos1 bb1))
         (sprite1-left (x pos1))
         (sprite1-right (vecx+ pos1 bb1))

         (sprite2-center (sprite-center-pos s2)))
    (and (>= (x sprite2-center) sprite1-left)
         (<= (y sprite2-center) sprite1-top)
         (<= (x sprite2-center) sprite1-right)
         (>= (y sprite2-center) sprite1-bottom))))
         

(defmacro define-fonts (font name &rest forms)
  `(progn
     (gamekit:define-font ,name ,font)
     (sleep 0.1)
     ,@(loop for f in forms
             collect `(defvar ,(car f)
                        (gamekit:make-font ,name ,(cadr f))))))

(defmacro draw-text (text pos &key (font gamekit::*font*) (color (vec4 0 0 0 1)))
  `(multiple-value-bind (_ text-width text-height) (gamekit:calc-text-bounds ,text ,font)
     (declare (ignore _)
              (ignorable text-height))
     (gamekit:draw-text ,text (vec2 (- (x ,pos) (/ text-width 2))
                                   (y ,pos))
                        :fill-color ,color :font ,font)))
