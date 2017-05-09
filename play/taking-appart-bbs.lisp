(in-package :fraggle)

(defun-g plot ((st :vec2) (pct :float))
  (- (smoothstep (- pct 0.01) pct (y st))
     (smoothstep pct (+ pct 0.01) (y st))))

(defun-g fraggle-quad-vert ((pos :vec2))
  (values (v! pos 0 1)
          (* (+ pos (v2! 1f0)) 0.5)))

(defun-g blum-blum-shub-hash-low-quality ((grid-cell :vec2))
  (let* ((hash-coord
          (nineveh.hashing::bbs-coord-prepare
           (v! (s~ grid-cell :xy) (+ (s~ grid-cell :xy) (v2! 1.0)))))
         (p (nineveh.hashing::bbs-permute (s~ hash-coord :xzxz))))
    (nineveh.hashing::bbs-permute-and-resolve (+ p (s~ hash-coord :yyww)))
    (/ (v! (s~ grid-cell :xy) (+ (s~ grid-cell :xy) (v2! 1.0))) 1000)))

(defun-g fraggle-quad-frag ((uv :vec2)
                         &uniform (now :float)
                         (mouse :vec2) (mouse-norm :vec2) (mouse-buttons :vec2)
                         (screen-res :vec2))
  (v! (x mouse-norm) (x mouse-buttons) (y mouse-norm) 1)
  (let ((grid-cell (floor (* uv 200))))
    (let* ((hash-coord
            (nineveh.hashing::bbs-coord-prepare
             (v! (s~ grid-cell :xy) (+ (s~ grid-cell :xy) (v2! 1.0)))))
           (v4 (s~ hash-coord :xzxz))
           (p (nineveh.hashing::mod-fixed-denominator-low-quality (* v4 v4)
                                                                  61f0)))
      ;; (nineveh.hashing::bbs-permute-and-resolve (+ p (s~ hash-coord :yyww)))
      ;; (/ (v! (s~ grid-cell :xy) (+ (s~ grid-cell :xy) (v2! 1.0))) 1000)
      (quick-mix (plot uv (/ (x v4) 200))
                 (x (/ v4 200))))))


(def-g-> draw-fraggle ()
  :vertex (fraggle-quad-vert :vec2)
  :fragment (fraggle-quad-frag :vec2))


(defun step-fraggle ()
  (ensure-fraggle-initialized)
  (as-frame
    (map-g #'draw-fraggle (get-quad-stream-v2)
           :now (* (now) 0.001)
           :mouse (get-mouse)
           :mouse-norm (get-mouse-norm)
           :mouse-buttons (get-mouse-buttons)
           :screen-res (viewport-resolution (current-viewport)))))


(def-simple-main-loop fraggle ()
  (step-fraggle))
