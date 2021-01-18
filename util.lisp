(in-package :fist)

(defstruct sprite
  image pos size center)

;; TODO: substitute _ for -
(defun to-keyword (thing)
  (intern (string-upcase thing)
          :keyword))

(defmethod vec- ((v1 vec2) (v2 vec2))
  (vec2 (- (x v1) (x v2))
        (- (y v1) (y v2))))

(defun move-fist (dir)
  (let ((pos (sprite-pos *fist*)))
    (setf (sprite-pos *fist*)
          (case dir
            (:right (vec2 (min *width* (+ (x pos) *speed*))
                          (y pos)))
            (:left (vec2 (max 0 (- (x pos) *speed*))
                          (y pos)))))))

(defmacro l (&body body)
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
