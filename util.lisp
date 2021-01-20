(in-package :fist)

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

(defmacro l (&body body)
  "shorthand way to write lambdas. accepts two arguments _ and __"
  `(lambda (&optional _ __)
     (declare (ignorable _ __))
     ,@body))
