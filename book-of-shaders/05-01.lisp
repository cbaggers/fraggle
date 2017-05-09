(in-package :fraggle)



(defun-g plot ((st :vec2) (pct :float))
  (- (smoothstep (- pct 0.02) pct (y st))
     (smoothstep pct (+ pct 0.02) (y st))))


(defun-g fraggle-quad-frag ((uv :vec2)
                         &uniform (now :float)
                         (mouse :vec2) (mouse-norm :vec2) (mouse-buttons :vec2)
                         (screen-res :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy) screen-res))
         (y (x st))
         (color (v3! y))
         (pct (plot st y))
         (final (+ (* (- 1f0 pct) color)
                   (* pct (v! 0 1 0)))))
    final))
