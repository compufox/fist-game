(in-package :fist)

(declaim (inline to-keyword random-side))

;; TODO: substitute _ for -
(defun to-keyword (thing)
  "convert THING to keyword"
  (intern (string-upcase thing)
          :keyword))

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
  (let ((bb1 (sprite-bounding s1))
        (bb2 (sprite-bounding s2))
        (pos1 (sprite-pos s1))
        (pos2 (sprite-pos s2)))
    (and (and (>= (x pos1) (x pos2))
              (<= (+ (x pos1) (x bb1))
                  (+ (x pos2) (x bb2))))
         (and (>= (y pos1) (y pos2))
              (<= (+ (y pos1) (y bb1))
                  (+ (y pos2) (y bb2)))))))

(defmacro define-fonts (font name &rest forms)
  `(progn
     (gamekit:define-font ,name ,font)
     ,@(loop for f in forms
             collect `(defvar ,(car f)
                        (gamekit:make-font ,name ,(cadr f))))))
