(in-package :fraggle)

;;------------------------------------------------------------

(defun-g fraggle-quad-vert ((pos :vec2))
  (values (v! pos 0 1)
          (* (+ pos (v2! 1f0)) 0.5)))

(defun-g fraggle-quad-frag ((uv :vec2)
                            &uniform (now :float)
                            (mouse :vec2)
                            (mouse-norm :vec2)
                            (mouse-buttons :vec2)
                            (screen-res :vec2))
  (let ((half-res (/ screen-res 2)))
    (flet ((func ((p :vec2))
             (merge-simple
              (merge-simple
               (circle (translate p (v! 200 150)) 80)
               (triangle (rotate-ccw p now)
                         10))
              (semicircle (rotate-ccw p (* 0.5 now))
                          80 120 20))))
      (let* ((mouse-pos (- mouse half-res))
             (p (- (s~ gl-frag-coord :xy)
                   half-res))
             (p (* p 1))
             (dist (func p))
             (light-col (apply-luminance
                         #'(rgb->luma-bt601 :vec4)
                         0.6
                         (vec4 0.75 1.0 0.5 1.0)))
             (light (point-light #'func
                                 p
                                 mouse-pos
                                 light-col
                                 250.0
                                 16.0)))
        (mix
         (mix
          (mix
           (+ (v! 0.03 0.03 0.04 1)
              light)
           (v! 0.7 0 0 1)
           (mask-border-outer dist 3))
          (v! 0 0.1 0.8 1)
          (mask-border-inner dist 3))
         (v! 0 0 0 0)
         (mask-fill dist))))))


(defpipeline-g draw-fraggle ()
  :vertex (fraggle-quad-vert :vec2)
  :fragment (fraggle-quad-frag :vec2))

;;------------------------------------------------------------

(defun step-fraggle ()
  (as-frame
    (map-g #'draw-fraggle (get-quad-stream-v2)
           :now (* (now) 0.001)
           :mouse (get-mouse)
           :mouse-norm (get-mouse-norm)
           :mouse-buttons (get-mouse-buttons)
           :screen-res (viewport-resolution (current-viewport)))))


(def-simple-main-loop fraggle
    (:on-start #'ensure-fraggle-initialized)
  (step-fraggle))

;;------------------------------------------------------------
