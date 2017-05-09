(in-package :fraggle)

(defun-g plot ((st :vec2) (pct :float))
  (- (smoothstep (- pct 0.02) pct (y st))
     (smoothstep pct (+ pct 0.02) (y st))))


(defun-g fraggle-quad-frag ((uv :vec2)
                         &uniform (now :float)
                         (mouse :vec2) (mouse-norm :vec2) (mouse-buttons :vec2)
                         (screen-res :vec2))
  (let ((color-a (v! 0.010 0.000 0.060))
        (color-b (v! 1 1 1))
        (pct (mod (* now 0.1) 1)))
    (v! (mix color-a color-b (nineveh.easing:in-bounce pct)) 1)))
