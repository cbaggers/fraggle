(in-package :fraggle)

;;------------------------------------------------------------
;; GPU Funcs
;;
;; If these end up being used a lot then move them to Nineveh

(defun-g 11to01 ((x :float))
  (+ 0.5 (* 0.5 x)))

(defun-g 11to01 ((x :vec2))
  (+ (v2! 0.5) (* 0.5 x)))

(defun-g 11to01 ((x :vec3))
  (+ (v3! 0.5) (* 0.5 x)))

(defun-g 11to01 ((x :vec4))
  (+ (v4! 0.5) (* 0.5 x)))

;;---

;; In nineveh I want the full GL blending functions setup
(defun-g quick-mix ((a :float) (b :float))
  (+ (* (- 1f0 a) b)
     a))

(defun-g quick-mix ((a :vec2) (b :vec2))
  (+ (* (- (v2! 1f0) a) b)
     a))

(defun-g quick-mix ((a :vec3) (b :vec3))
  (+ (* (- (v3! 1f0) a) b)
     a))

(defun-g quick-mix ((a :vec4) (b :vec4))
  (+ (* (- (v4! 1f0) a) b)
     a))

;;---
