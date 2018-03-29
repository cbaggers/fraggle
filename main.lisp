(in-package :fraggle)

;;
;;============================================================
;; Based on Maarten's fantastic 2D sdf function playground:
;; https://www.shadertoy.com/view/4dfXDn
;;============================================================
;;

;;------------------------------------------------------------
;; Combine distance field functions

(defun-g merge-smooth ((d1 :float) (d2 :float) (k :float))
  (let ((h (clamp (+ 0.5
                     (* 0.5
                        (/ (- d2 d1)
                           k)))
                  0.0
                  1.0)))
    (- (mix d2 d1 h)
       (* k h (- 1.0 h)))))

(defun-g merge-simple ((d1 :float) (d2 :float))
  (min d1 d2))

(defun-g merge-exclude ((d1 :float) (d2 :float))
  (min (max (- d1) d2)
       (max (- d2) d1)))

(defun-g subtract ((d1 :float) (d2 :float))
  (max (- d1) d2))

(defun-g intersect ((d1 :float) (d2 :float))
  (max d1 d2))

;;------------------------------------------------------------
;; Masks for drawing

(defun-g mask-fill ((dist :float))
  (clamp (- dist) 0.0 1.0))

(defun-g mask-border-inner ((dist :float) (width :float))
  (let* ((alpha1 (clamp (+ dist width) 0.0 1.0))
         (alpha2 (clamp dist 0.0 1.0)))
    (- alpha1 alpha2)))

(defun-g mask-border-outer ((dist :float) (width :float))
  (let* ((alpha1 (clamp dist 0.0 1.0))
         (alpha2 (clamp (- dist width) 0.0 1.0)))
    (- alpha1 alpha2)))

;;------------------------------------------------------------
;; Light & Shadow

(defun-g cast-shadow ((fn (function (:vec2) :float))
                      (p :vec2)
                      (pos :vec2)
                      (radius :float))
  (let* ((dir (normalize (- pos p)))
         ;; distance to light
         (dl (length (- p pos)))
         ;; fraction of light visible, starts at one radius
         ;; (second half added in the end)
         (lf (* radius dl))
         ;; distance traveled
         (dt 0.01))
    (dotimes (i 64)
      (let (;; distance to scene at current position
            (sd (funcall fn (+ p (* dir dt)))))
        ;; early out when this ray is guaranteed to be full shadow
        (when (< sd (- radius))
          (return 0.0))
        ;; width of cone-overlap at light
        ;; 0 in center, so 50% overlap: add one radius outside of loop to
        ;; get total coverage.
        ;; should be '(sd / dt) * dl', but '*dl' outside of loop
        (setf lf (min lf (/ sd dt)))
        ;; move ahead
        (incf dt (max 1.0 (abs sd)))
        (when (> dt dl)
          (break))))
    ;; multiply by dl to get the real projected overlap (moved out of loop)
    ;; add one radius, before between -radius and + radius
    ;; normalize to 1 ( / 2*radius)
    (setf lf (clamp (/ (+ (* lf dl) radius) (* 2.0 radius))
                    0.0
                    1.0))
    (setf lf (smoothstep 0.0 1.0 lf))
    lf))

(defun-g point-light ((fn (function (:vec2) :float))
                     (p :vec2)
                     (pos :vec2)
                     (color :vec4)
                     (dist :float)
                     (range :float)
                     (radius :float))
  (let* (;; distance to light
         (ld (length (- p pos))))
    (if (> ld range)
        (vec4 0.0)
        (let* ((shad (cast-shadow fn p pos radius))
               (fall (/ (- range ld) range))
               (fall (* fall fall))
               (source (mask-fill (circle (- p pos) radius))))
          (* (+ (* shad fall) source)
             color)))))
;;------------------------------------------------------------
;; Distance Field Functions

(defun-g circle ((p :vec2) (radius :float))
  (- (length p) radius))

(defun-g triangle ((p :vec2) (radius :float))
  (- (max (+ (* (x (abs p)) 0.866025)
             (* (y p) 0.5))
          (- (y p)))
     (* 0.5 radius)))

(defun-g triangle ((p :vec2) (width :float) (height :float))
  (let ((n (normalize (vec2 height (/ width 2.0)))))
    (max (- (+ (* (x (abs p)) (x n))
               (* (y p) (y n)))
            (* height (y n)))
         (- (y p)))))

(defun-g pie ((p :vec2) (angle :float))
  (let* ((angle (/ (radians angle) 2.0))
         (n (vec2 (cos angle) (sin angle))))
    (+ (* (x (abs p)) (x n))
       (* (y p) (y n)))))

(defun-g semicircle ((p :vec2)
                     (radius :float)
                     (angle :float)
                     (width :float))
  (let* ((width (/ width 2f0))
         (radius (- radius width)))
    (subtract (pie p angle)
              (- (abs (circle p radius)) width))))

(defun-g rectangle ((p :vec2) (size :vec2) (radius :float))
  (let* ((size (- size (vec2 radius)))
         (d (- (abs p) size)))
    (- (+ (min (max (x d) (y d)) 0.0)
          (length (max d 0.0)))
       radius)))

(defun-g line ((p :vec2) (start :vec2) (end :vec2) (width :float))
  (let* ((dir (- start end))
         (length (length dir))
         (dir (/ dir length))
         (proj (* (max 0.0 (min length (dot (- start p) dir)))
                  dir)))
    (- (length (- start p proj))
       (/ width 2.0))))

;;------------------------------------------------------------

(defun-g rotate-ccw ((p :vec2) (a :float))
  (let ((m (mat2 (cos a) (sin a)
                 (- (sin a)) (cos a))))
    (* m p)))

(defun-g translate ((p :vec2) (o :vec2))
  (- p o))

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
             (mix (mix (circle p 80)
                       (triangle (rotate-ccw p now)
                                 80)
                       (y mouse-norm))
                  (semicircle (rotate-ccw p now)
                              80 120 20)
                  (saturate (* (x mouse-norm) 2)))))
      (let* ((mouse-pos (- mouse half-res))
             (p (- (s~ gl-frag-coord :xy)
                   half-res))
             (p (* p 1))
             (dist (func p))
             (light (point-light #'func
                                p
                                mouse-pos
                                (vec4 0.75 1.0 0.5 1.0)
                                dist
                                250.0
                                6.0)))
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
