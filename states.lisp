(in-package :fist)

(defclass menu-state ()
  ((options :initform '("Start" "Quit")
            :reader options)
   (cutscene-viewed :initform nil
                    :accessor cutscene)
   (music :initform nil)
   (selected :initform 0
             :accessor selected)))

(defclass playing-state ()
  ((paused :initform nil
           :accessor paused)
   (tick :initform 0
         :accessor tick)
   (succeeded :initform nil
              :accessor succeeded)
   (failed :initform nil
           :accessor failed)))

;; MENU SCREEN
(defmethod gamekit:post-initialize ((this menu-state))
  (unless (and *large-font* *small-font*)
    (setf *large-font* (gamekit:make-font :krona 50)
          *small-font* (gamekit:make-font :krona 20)))
  (setf (cutscene this) nil)
  (with-slots (selected music) this
    (unless music
      (gamekit:play-sound :music :looped-p t)
      (setf music t))
    
    (gamekit:bind-button :left :pressed
      (l (setf selected (mod (1- selected) (length (options this))))))
    (gamekit:bind-button :right :pressed
      (l (setf selected (mod (1+ selected) (length (options this))))))
    (gamekit:bind-button :enter :pressed
      (l (case selected
           ;; for state 0 i want to do a little cutscene,
           ;;  before transitioning
           (0 (setf (cutscene this) t))
           (1 (gamekit:stop))
           (t (gamekit:stop))))))
  (when (and *moon* *fist* *flame*)
    (set-sprite-menu-position)))

(defmethod gamekit:pre-destroy ((this menu-state))
  (setf (sprite-pos *fist*) (vec2 (/ +width+ 2) 0)
        (sprite-scale *fist*) (vec2 1 1)
        (sprite-scale *flame*) (vec2 1 1)
        (sprite-scale *moon*) (vec2 1 1)
        (sprite-pos *moon*) (vec2 (random +width+) *max-distance*)
        (cutscene this) nil)
  (set-sprite-velocity *fist*))

;; GAME STATE
(defmethod gamekit:post-initialize ((this playing-state))
  
  ;; cleanup
  (loop for sprite in *sprites*
        unless (eql (sprite-image sprite) :game-bg)
          do (setf (sprite-render sprite) nil))
  (setf (sprite-render *flame*) t
        (sprite-render *fist*) t
        (sprite-render *moon*) t
        (sprite-vel *moon*) +origin+)
  (set-sprite-velocity *moon* :dir :down :speed *fist-speed*)

  ;; key bindings
  (gamekit:bind-button :right :pressed
    (l (set-sprite-velocity *fist* :dir :right :speed *fist-speed*)))
  (gamekit:bind-button :left :pressed
    (l (set-sprite-velocity *fist* :dir :left :speed *fist-speed*)))
  (gamekit:bind-button :up :pressed
    (l (set-sprite-velocity *fist* :dir :up :speed *fist-speed*)))
  (gamekit:bind-button :down :pressed
    (l (set-sprite-velocity *fist* :dir :down :speed *fist-speed*)))
  (gamekit:bind-button :right :released
    (l (set-sprite-velocity *fist*)))
  (gamekit:bind-button :left :released
    (l (set-sprite-velocity *fist*)))
  (gamekit:bind-button :up :released
    (l (set-sprite-velocity *fist*)))
  (gamekit:bind-button :down :released
    (l (set-sprite-velocity *fist*)))
  
  (gamekit:bind-button :enter :pressed
    (l (when (or (succeeded this)
                 (failed this))
         (gamekit.fistmachine:transition-to 'menu-state))))
  (gamekit:bind-button :escape :pressed
    (l (if (or (succeeded this)
               (failed this))
           (gamekit:stop)
           (setf (paused this) (not (paused this)))))))

(defmethod gamekit:pre-destroy ((this playing-state))
  (setf (sprite-render *flame*) nil
        (succeeded this) nil
        (failed this) nil)
  (set-sprite-menu-position))
