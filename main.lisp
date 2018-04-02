(in-package :fraggle)

(defvar *ssbo* nil)
(defvar *img* nil)

(defun-g fraggle-quad-vert ((pos :vec2))
  (values (v! pos 0 1)
          (* (+ pos (v2! 1f0)) 0.5)))


(defstruct-g picked
  (col :vec4)
  (avg :vec4))


(defun-g fraggle-quad-frag ((uv :vec2)
                            &uniform (now :float)
                            (mouse :vec2)
                            (mouse-norm :vec2)
                            (mouse-buttons :vec2)
                            (screen-res :vec2)
                            (img :sampler-2d)
                            (ssbo picked :ssbo))
  (let* ((half-res (/ screen-res 2))

         (suv (* (v! 1 -1) uv))
         (snext (/ screen-res))
         (mpos (- mouse half-res))
         (mpos-norm mouse-norm)
         (spos (* (vec2 1 -1) mpos-norm))
         (col (texture img spos))
         (avg (/ (+ (texture img (+ spos (* (vec2 -1  1) snext)))
                    (texture img (+ spos (* (vec2  0  1) snext)))
                    (texture img (+ spos (* (vec2  1  1) snext)))
                    (texture img (+ spos (* (vec2 -1  0) snext)))
                    col
                    (texture img (+ spos (* (vec2  1  0) snext)))
                    (texture img (+ spos (* (vec2 -1 -1) snext)))
                    (texture img (+ spos (* (vec2  0 -1) snext)))
                    (texture img (+ spos (* (vec2  1 -1) snext))))
                 9.0))

         (p (- (s~ gl-frag-coord :xy) half-res))
         (dist (sdf:semicircle (sdf:translate p mpos) 50 (radians 100) 15))
         (dist2 (sdf:semicircle (sdf:translate p mpos) 80 (radians 300) 15)))
    (setf (picked-col ssbo) col)
    (setf (picked-avg ssbo) col)
    (mix (mix (mix (mix (texture img suv)
                        col
                        (sdf:mask-fill dist))
                   (v! 0.3 0.3 0.3 0)
                   (sdf:mask-border-inner dist 2))
              avg
              (sdf:mask-fill dist2))
         (v! 0.3 0.3 0.3 0)
         (sdf:mask-border-inner dist2 2))))

(defpipeline-g draw-fraggle ()
  :vertex (fraggle-quad-vert :vec2)
  :fragment (fraggle-quad-frag :vec2))


(defun step-fraggle ()
  (as-frame
    (map-g #'draw-fraggle (get-quad-stream-v2)
           :now (* (now) 0.001)
           :mouse (get-mouse)
           :mouse-norm (get-mouse-norm)
           :mouse-buttons (get-mouse-buttons)
           :screen-res (viewport-resolution (current-viewport))
           :img *img*
           :ssbo *ssbo*)
    (when (mouse-down-p mouse.left)
      (let ((frame (frame-at-point (mouse-pos (mouse)))))
        (when (not (eq frame *last-at-point*))
          (print frame)
          (setf *last-at-point* frame))))
    (clear)
    (tvp-layout)
    (tvp-draw)))


(def-simple-main-loop fraggle
    (:on-start #'ensure-fraggle-initialized)
  (step-fraggle))
