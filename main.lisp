(in-package :fraggle)

;;------------------------------------------------------------

(define-pgraph plain (:height-color)
    (coord (now :float))
  (let* ((time (/ now 1000))
         (height 10)
         (wave (- (max (* (log (* (- (x coord) (+ 100 (* 120 (sin time))))
                                  0.5))
                          5)
                       0)
                  10))
         (val (max (* 2 (+ (* 10 (perlin-noise (* coord 0.04)))
                           (* 6 (perlin-noise (* coord 0.09)))))
                   wave)))
    (values
     val
     (mix (v! 0 0.2 0.4 0)
          (v! 0.3 0.3 0.3 0)
          (step wave (- val 0.1))))))

;;------------------------------------------------------------

(defparameter *rot* (q:identity))
(defparameter *dir* (v! 0 0 -1))
(defparameter *pos* (v! -10 80 200))

(defun update-controls ()
  (let* ((mmove (mouse-move (mouse)))
         (xmove (* (x mmove) 0.01))
         (ymove (* (y mmove) 0.01))
         (yax (q:from-axis-angle (v! 0 1 0) (- xmove)))
         (xax (q:from-axis-angle (v! 1 0 0) (- ymove)))
         (rot (if (mouse-down-p mouse.left)
                  (q:normalize (q:* *rot* (q:normalize (q:* xax yax))))
                  *rot*))
         (dir (q:to-direction *rot*)))
    (when (key-down-p key.w)
      (v3:incf *pos* (v3:*s *dir* 1f0)))
    (when (key-down-p key.s)
      (v3:incf *pos* (v3:*s *dir* -1f0)))
    (setf *rot* rot)
    (setf *dir* dir))
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
          :point-color (v! 0.3 0.3 0.3 0)
          :point-size 0.4
          :spacing 3.4
          :now (now))))


(def-simple-main-loop fraggle
    (:on-start #'ensure-fraggle-initialized)
  (step-fraggle))

;;------------------------------------------------------------
