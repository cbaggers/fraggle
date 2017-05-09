(in-package :fraggle)

(defun-g plot ((st :vec2) (pct :float))
  (- (smoothstep (- pct 0.02) pct (y st))
     (smoothstep pct (+ pct 0.02) (y st))))


(defun-g fraggle-quad-frag ((uv :vec2)
                         &uniform (now :float)
                         (mouse :vec2) (mouse-norm :vec2) (mouse-buttons :vec2)
                         (screen-res :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy) screen-res))
         (x (x st))
         ;;
         (color-a (v! 0.912 0.341 0.084))
         (color-b (v! 1 0.807 0.390))
         ;;
         (pct x)
         ;;
         (pct (v3! pct))
         (color (mix color-a color-b pct))
         (color (mix color (v! 1 0 0) (plot st (x pct))))
         (color (mix color (v! 0 1 0) (plot st (y pct))))
         (color (mix color (v! 0 0 1) (plot st (z pct)))))
    (v! color 1)))
