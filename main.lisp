(in-package :fraggle)

;;------------------------------------------------------------

(define-pgraph woop (:range)
    (i (now :float))
  (let* ((ang (* i 0.01))
         (disp (* (vec2 (sin ang) (cos ang))
                  (* i 0.002))))
    (vec3 (x disp)
          (* (+ 40 (* 2 (cos (* (+ (* 0.1 i) now) 0.001))))
             (sin (* 0.1 i)))
          (y disp))))

(define-pgraph boop (:height-color)
    (coord (now :float))
  (let* ((now (* now 0.1))
         (pos (- coord (vec2 100)))
         ;;
         (dist (merge-smooth
                (merge-smooth (triangle pos
                                        50)
                              (circle (- pos (* (vec2 (sin (* now 0.18))
                                                      (* (cos (* 1 now)) 1.4))
                                                60))
                                      10)
                              10f0)
                (circle (- pos (* (vec2 (sin now)
                                        (cos now))
                                  50))
                        20)
                10))
         (dist-col (vec4 dist))
         ;;
         (masked (mask-fill dist))
         (masked-col (mix (vec4 0)
                          (vec4 0.7)
                          masked))
         ;;
         (final-val masked)
         (final-col masked-col))
    ;;
    (values dist
            masked-col)))

(define-pgraph plain (:height)
    (coord (at :float))
  at)

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
    ;; (woop *pos*
    ;;       *dir*
    ;;       :min 0
    ;;       :max 200000
    ;;       :point-size 1.3
    ;;       :point-color (v! 0.1 0.1 0.19 1)
    ;;       :now (float (now) 0f0))
    (boop *pos*
          *dir*
          :x-min 0
          :x-max 200
          :y-min 0
          :y-max 200
          :by 1
          :point-color (v! 0.1 0.1 0.19 1)
          :now (* 0.01 (now))
          :spacing 1.4)

    (plain *pos*
          *dir*
          :x-min 0
          :x-max 200
          :y-min 0
          :y-max 200
          :by 1
          :point-color (v! 0.2 0.01 0.01 1)
          :at 0.0
          :spacing 1.4)))


(def-simple-main-loop fraggle
    (:on-start #'ensure-fraggle-initialized)
  (step-fraggle))

;;------------------------------------------------------------
