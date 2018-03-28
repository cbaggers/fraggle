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
             (merge-simple
              (mix (mix (circle p 180)
                        (triangle (rotate-ccw p now)
                                  180)
                        (y mouse-norm))
                   (rectangle p (v! 100 100) 10)
                   (x mouse-norm))
              (circle (translate p (- mouse half-res)) 50))))
      (let* ((p (- (s~ gl-frag-coord :xy)
                   half-res))
             (p (* p 1)))
        (mix (mix (mix (v! 0.03 0.03 0.04 1) (v! 0.7 0 0 1)
                       (mask-border-outer (func p) 3))
                  (v! 0 0.1 0.8 1)
                  (mask-border-inner (func p) 3))
             (v! 0 0 0 0)
             (mask-fill (func p)))))))


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
