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

;;------------------------------------------------------------

(defparameter *rot* (q:identity))
(defparameter *dir* (v! 0 0 -1))
(defparameter *pos* (v! -10 80 200))

(defun update-controls ()
  (let* ((mmove (mouse-move (mouse)))
         (xmove (* (x mmove) 0.01))
         (ymove (* (y mmove) 0.01))
         (yax (q:from-axis-angle (v! 0 1 0) xmove))
         (xax (q:from-axis-angle (v! 1 0 0) ymove))
         (rot (q:* *rot* (q:* yax xax)))
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
    (woop *pos*
          *dir*
          :min 0
          :max 200000
          :point-size 1.3
          :point-color (v! 0.1 0.1 0.19 1)
          :now (float (now) 0f0))))


(def-simple-main-loop fraggle
    (:on-start #'ensure-fraggle-initialized)
  (step-fraggle))

;;------------------------------------------------------------
