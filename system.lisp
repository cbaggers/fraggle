(in-package :fraggle)

;; init

(defvar *initd* nil)

(defun ensure-fraggle-initialized ()
  (unless *initd*
    ;; we need an gl-initialized-p function in cepl
    (unless cepl.context::*gl-context*
      (cepl:repl))
    (skitter:listen-to (lambda (x y z)
                         (declare (ignore z))
                         (window-size-callback x y))
                       (skitter:window 0) :size)
    (setf *initd* t)))

;; windows

(defun window-size-callback (size-2d y)
  (declare (ignore y))
  (setf (viewport-resolution (current-viewport))
        (v! (skitter:size-2d-vec size-2d))))

;; mouse

(defun get-mouse-norm ()
  (let ((pos (v2:/ (skitter:xy-pos-vec (skitter:mouse-pos (skitter:mouse 0)))
                   (viewport-resolution (current-viewport)))))
    (v! (x pos) (- 1f0 (y pos)))))

(defun get-mouse ()
  (let ((pos (skitter:xy-pos-vec (skitter:mouse-pos (skitter:mouse 0))))
        (h (y (viewport-resolution (current-viewport)))))
    (v! (x pos) (- h (y pos)))))

(defun get-mouse-buttons ()
  (v! (if (skitter:mouse-down-p mouse.left (skitter:mouse 0)) 1f0 0f0)
      (if (skitter:mouse-down-p mouse.right (skitter:mouse 0)) 1f0 0f0)))

;; time

(let ((offset 0))

  (defun now ()
    (- (get-internal-real-time) offset))

  (defun reset-time ()
    (setf offset (get-internal-real-time))))


;; blend-color

(defun-g blend ((a :float) (b :float))
  (+ (* a (- 1 b)) b))

(defun-g blend ((a :vec2) (b :vec2))
  (+ (* a (- (v2! 1) b)) b))

(defun-g blend ((a :vec3) (b :vec3))
  (+ (* a (- (v3! 1) b)) b))

(defun-g blend ((a :vec4) (b :vec4))
  (+ (* a (- (v4! 1) b)) b))
