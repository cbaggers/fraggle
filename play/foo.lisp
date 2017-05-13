(in-package :fraggle)

(defun-g foo ((x :float))
  (sin (/ 1 (tan x))))

(defun-g fraggle-quad-frag ((uv :vec2)
                            &uniform (now :float)
                            (mouse :vec2)
                            (mouse-norm :vec2)
                            (mouse-buttons :vec2)
                            (screen-res :vec2))
  (v3! (graph #'sin uv 0.009)))
