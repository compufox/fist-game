(in-package :fist)

(defun move-fist (dir)
  "move the fist in a direction"
  (let ((pos (sprite-pos *fist*)))
    (setf (sprite-pos *fist*)
          (case dir
            (:right (vec2 (min *width* (+ (x pos) *speed*))
                          (y pos)))
            (:left (vec2 (max 0 (- (x pos) *speed*))
                          (y pos)))))))
