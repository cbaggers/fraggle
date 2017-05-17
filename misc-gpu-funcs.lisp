(in-package :fraggle)

;;------------------------------------------------------------
;; GPU Funcs
;;
;; If these end up being used a lot then move them to Nineveh
;;
;; (decided to make 'remap' in nineveh)

(defun-g 11to01 ((x :float))
  (+ 0.5 (* 0.5 x)))

(defun-g 11to01 ((x :vec2))
  (+ (v2! 0.5) (* 0.5 x)))

(defun-g 11to01 ((x :vec3))
  (+ (v3! 0.5) (* 0.5 x)))

(defun-g 11to01 ((x :vec4))
  (+ (v4! 0.5) (* 0.5 x)))

;; blend-color

(defun-g blend ((src :float) (dst :float))
  (+ src (* dst (- 1f0 src))))

(defun-g blend ((src :vec2) (dst :vec2))
  (+ src (* dst (- (v2! 1f0) src))))

(defun-g blend ((src :vec3) (dst :vec3))
  (+ src (* dst (- (v3! 1f0) src))))

(defun-g blend ((src :vec4) (dst :vec4))
  (let ((src-rgb (s~ src :xyz))
        (dst-rgb (s~ dst :xyz))
        (src-alpha (w src))
        (dst-alpha (w dst)))
    (v! (+ src-rgb (* dst-rgb (- (v3! 1) src-rgb)))
        (+ src-alpha (* dst-alpha (- 1f0 src-alpha))))))
