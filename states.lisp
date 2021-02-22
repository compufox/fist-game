(in-package :fist)

(defclass menu-state ()
   ((music :initform nil)
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
  (unless *start-menu*
    (setf *start-menu*
          (make-menu "ROCKET FIST" nil
                     #'menu-callback
                     ;:position (vec2 400 800)
                     :heading-font *large-font*
                     :option-font *small-font*
                     :fill-color +white+))
    (initialize-menu *start-menu*))
  
  (with-slots (music) this
    (unless music
      (gamekit:play-sound :music :looped-p t)
      (setf music t)))
  (when (and *moon* *fist* *flame*)
    (set-sprite-menu-position))

  (when *start-menu*
    (restart-timeline *menu-timeline*)
    (play-timeline *menu-timeline*)))

(defmethod gamekit:pre-destroy ((this menu-state))
  (setf (sprite-pos *fist*) (vec2 (/ +width+ 2) 0)
        (sprite-scale *fist*) (vec2 1 1)
        (sprite-scale *flame*) (vec2 1 1)
        (sprite-scale *moon*) (vec2 1 1)
        (sprite-pos *moon*) (vec2 (random +width+) *max-distance*))
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
