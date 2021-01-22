(in-package :fist)

(declaim (inline move-sprite))

(defun process-collision ()
  ;; every rendered sprite
  ;;  check if they have collision on
  ;;  get what group theyre in, and what groups they collide with
  ;;  check if bounding boxes are overlapping with any other sprite
  ;;  
  ;;  if collisions are happening, process them.
  (let ((sprites (loop for s in *sprites*
                       when (and (sprite-render s)
                                 (sprite-collision s))
                         collect s)))
    (loop for s1 in sprites
          do (loop for s2 in sprites
                   unless (equalp s1 s2) do
                     (when (and (member (sprite-collision-group s1)
                                        (sprite-collide-with s2))
                                (sprites-overlap-p s1 s2))
                       (format t "collided!"))))))

(defun move-sprite (s)
  (setf (sprite-pos s)
        (vec+ (sprite-pos s) (sprite-vel s))))

(defun set-sprite-velocity (sprite dir speed)
  "move a sprite in a direction"
  (setf (sprite-vel sprite)
        (vec+ (sprite-vel sprite)
              (case dir
                (:right (vec* +right+ speed))
                (:left (vec* +left+ speed))
                (:up (vec* +up+ speed))
                (:down (vec* +down+ speed))))))

(defun maybe-spawn-enemy ()
  (when (and (< *enemy-count* +max-enemies+)
             (zerop (mod *current-tick* +enemy-spawn-delay+)))
    (let ((s (get-random-from-pool *ufo-pool* *star1-pool*))
          (starting-pos (vec2 (random-side) (random +height+))))
      (when s
        (setf (sprite-pos s) starting-pos
              (sprite-render s) t
              (sprite-vel s) +origin+)
        (set-sprite-velocity s (if (> (x starting-pos) (/ +width+ 2))
                                   :left
                                   :right)
                             (1+ (random 8)))))))

(defun maybe-hide-enemies ()
  (loop for s in (append (spritepool-pool *ufo-pool*)
                         (spritepool-pool *star1-pool*))
        for pos = (sprite-pos s)
        
        when (and (sprite-render s)
                  (or 
                   (> (x pos) +width+)
                   (< (x pos) 0)))
          do (setf (sprite-render s) nil)))

(defun process ()
;  (move-enemies)
  (maybe-hide-enemies)
  (maybe-spawn-enemy)
  (process-collision)

  (loop for sprite in *sprites*
        when (sprite-render sprite)
          do (move-sprite sprite))
  
  (setf (sprite-pos *flame*)
        (vec- (sprite-pos *fist*)
              (vec2 5 46))))
