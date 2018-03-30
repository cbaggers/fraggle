(in-package :fraggle)

;;------------------------------------------------------------

(define-pgraph plain (:height-color)
    (coord (now :float))
  (let* ((time (/ now 1000))
         (height 5)
         (wave (- (max (* (log (* (- (x coord) (+ 100 (* 120 (sin time))))
                                  0.5))
                          (* height 0.5))
                       0)
                  height))
         (val (min (max (* 2 (+ (* 10 (perlin-noise (* coord 0.04)))
                                (* 6 (perlin-noise (* coord 0.09)))))
                        wave
                        -3)
                   7)))
    (values
     (* val 1.3)
     (mix (v! 0 0.2 0.4 0)
          (v! 0.3 0.3 0.3 0)
          (step wave (- val 0.1))))))

;;------------------------------------------------------------

(defparameter *rot* (q:identity))
(defparameter *dir* (v! 0 0 -1))
(defparameter *pos* (v! -10 80 200))

(defun control-inner (turn-func-v2
                      move-func-float
                      turn-scale
                      move-scale)
  (let* ((mmove (v2:*s (funcall turn-func-v2) turn-scale))
         (yax (q:from-axis-angle (v! 0 1 0) (- (x mmove))))
         (xax (q:from-axis-angle (v! 1 0 0) (- (y mmove))))
         (fmove (* (funcall move-func-float) move-scale))
         (rot (q:normalize (q:* *rot* (q:normalize (q:* xax yax)))))
         (dir (q:to-direction *rot*)))
    (v3:incf *pos* (v3:*s *dir* fmove))
    (setf *rot* rot)
    (setf *dir* dir)))

(defun update-controls (&optional
                          (turn-scale 0.01f0)
                          (move-scale 1f0))
  ;; (control-inner (lambda ()
  ;;                  (if (mouse-down-p mouse.left)
  ;;                      (mouse-move (mouse))
  ;;                      (vec2 0f0 0f0)))
  ;;                (lambda ()
  ;;                  (let ((v 0f0))
  ;;                    (when (key-down-p key.w)
  ;;                      (incf v 1f0))
  ;;                    (when (key-down-p key.s)
  ;;                      (incf v -1f0))
  ;;                    v))
  ;;                turn-scale
  ;;                move-scale)
  (control-inner (lambda ()
                   (v2-n:* (v! 1.3 1.7)
                           (gamepad-2d (gamepad) 0)))
                 (lambda ()
                   (- (gamepad-1d (gamepad) 1)
                      (gamepad-1d (gamepad) 0)))
                 turn-scale
                 move-scale)
  (decay-events))

(defun step-fraggle ()
  (update-controls)
  (nineveh:as-frame
    (plain *pos*
          *dir*
          :x-min 0
          :x-max 200
          :y-min 0
          :y-max 200
          :by 0.7
          :point-size 1
          :spacing 1.8
          :now (now))))


(def-simple-main-loop fraggle
    (:on-start #'ensure-fraggle-initialized)
  (step-fraggle))

;;------------------------------------------------------------
