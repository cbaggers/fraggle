(in-package :fraggle)



(defun-g plot ((st :vec2) (pct :float))
  (- (smoothstep (- pct 0.02) pct (y st))
     (smoothstep pct (+ pct 0.02) (y st))))


(defun-g fraggle-quad-frag ((uv :vec2)
                         &uniform (now :float)
                         (mouse :vec2) (mouse-norm :vec2) (mouse-buttons :vec2)
                         (screen-res :vec2))
  ;;
  ;; We don't have the browsers graph software so we hack about here
  ;; the relationships arent as clear as the previous examples but we
  ;; can still see enough to get the intuition
  ;;
  (let* ((st (/ (s~ gl-frag-coord :xy) screen-res))
         (x (- (* (x st) (* 2 +pi+)) +pi+))
         ;;
         (y (sin x))
         ;;(y (fract x))
         ;;(y (fract (sin x)))
         ;;(y (ceil (sin x)))
         ;;(y (floor (sin x)))
         ;;(y (sign 10))
         ;;(y (abs (sin x)))
         ;;(y (clamp (sin x) -0.4 0.98))
         ;;(y (min 0 (sin x)))
         ;;(y (max 0 (sin x)))
         ;;
         (color (v3! y))
         (y (* 0.5 (+ 1 y)))
         (pct (plot st y))
         (final (+ (* (- 1f0 pct) color)
                   (* pct (v! 0 1 0)))))
    final))
