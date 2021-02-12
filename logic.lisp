(in-package :fist)

(declaim (inline move-sprite))

(defun process-collision (state)
  ;; every rendered sprite
  ;;  check if they have collision on
  ;;  get what group theyre in, and what groups they collide with
  ;;  check if bounding boxes are overlapping with any other sprite
  ;;  
  ;;  if collisions are happening, process them.
  (loop for s1 in (get-rendering-sprites)
        for colliding = (get-sprites-in-group (sprite-collide-with s1))

        do (loop for s2 in colliding
                 when (sprites-overlap-p s2 s1)
                   do (if (or (and (eql (sprite-image s1) :moon)
                                   (eql (sprite-image s2) :fist))
                              (and (eql (sprite-image s1) :fist)
                                   (eql (sprite-image s2) :moon)))
                          (progn
                            (unless (succeeded state)
                              (gamekit:play-sound :explosion2))
                            (setf (succeeded state) t))
                          (progn
                            (unless (failed state)
                              (gamekit:play-sound :explosion1))
                            (setf (failed state) t))))))

(defun get-sprites-in-group (collision-group)
  (loop for s in (get-rendering-sprites)
        when (member (sprite-collision-group s) collision-group)
          collect s))

(defun get-rendering-sprites ()
  (loop for s in *sprites*
        when (and (sprite-render s)
                  (sprite-collision s))
          collect s))

(defun move-sprite (s)
  (if (eql (sprite-image s) :fist)
      (player-movement-checks s)
      (setf (sprite-pos s)
            (vec+ (sprite-pos s) (sprite-vel s)))))

(defun player-movement-checks (p)
  (let* ((pos (sprite-pos p))
         (new-pos (vec+ pos (sprite-vel p))))
    (setf (sprite-pos p)
          (cond
            ((> (x new-pos) +width+)
             (vec2 -36 (y pos)))
            ((< (x new-pos) -36)
             (vec2 +width+ (y new-pos)))
            ((> (y new-pos) +height+)
             pos)
            ((< (y new-pos) 0)
             pos)
            (t new-pos)))))

(defun set-sprite-velocity (sprite &key dir speed)
  "move a sprite in a direction"
  (if (and dir speed)
      (setf (sprite-vel sprite)
            (vec+ (sprite-vel sprite)
                  (case dir
                    (:right (vec* +right+ speed))
                    (:left (vec* +left+ speed))
                    (:up (vec* +up+ speed))
                    (:down (vec* +down+ speed)))))
      (setf (sprite-vel sprite) +origin+)))

(defun maybe-spawn-enemy (state)
  (when (and (< *enemy-count* +max-enemies+)
             (zerop (mod (tick state) +enemy-spawn-delay+)))
    (let ((s (get-random-from-pool *ufo-pool* *star1-pool*))
          (starting-pos (vec2 (random-side) (random +height+))))
      (when s
        (setf (sprite-pos s) starting-pos
              (sprite-render s) t
              (sprite-vel s) +origin+)
        (set-sprite-velocity s :dir (if (> (x starting-pos) (/ +width+ 2))
                                        :left
                                        :right)
                             :speed (1+ (random 8)))))))

(defun maybe-hide-enemies ()
  (loop for s in (append (spritepool-pool *ufo-pool*)
                         (spritepool-pool *star1-pool*))
        for pos = (sprite-pos s)
        
        when (and (sprite-render s)
                  (or (> (x pos) +width+)
                      (< (x pos) 0)))
          do (setf (sprite-render s) nil)))

(defun process (state)
;  (move-enemies)
  (maybe-hide-enemies)
  (maybe-spawn-enemy state)
  (process-collision state)

  (loop for sprite in *sprites*
        when (sprite-render sprite)
          do (move-sprite sprite))
  
  (setf (sprite-pos *flame*)
        (vec- (sprite-pos *fist*)
              (vec2 5 46))))
