(in-package :fraggle)

;; init

(defvar *initd* nil)

(defun ensure-fraggle-initialized ()
  (unless *initd*
    ;; we need an gl-initialized-p function in cepl
    (unless cepl.context::*gl-context*
      (cepl:repl))
    (listen-to (lambda (size &rest ignored)
                 (declare (ignore ignored))
                 (window-size-callback size))
               (window 0) :size)
    (setf *initd* t)))

;; windows

(defun window-size-callback (size-2d)
  (setf (viewport-resolution (current-viewport))
        (v! size-2d)))

;; mouse

(defun get-mouse-norm ()
  (let ((pos (v2:/ (mouse-pos (mouse 0))
                   (viewport-resolution (current-viewport)))))
    (v! (x pos) (- 1f0 (y pos)))))

(defun get-mouse ()
  (let ((pos (mouse-pos (mouse 0)))
        (h (y (viewport-resolution (current-viewport)))))
    (v! (x pos) (- h (y pos)))))

(defun get-mouse-buttons ()
  (v! (if (mouse-down-p mouse.left) 1f0 0f0)
      (if (mouse-down-p mouse.right) 1f0 0f0)))

;; time

(let ((offset 0))

  (defun now ()
    (- (get-internal-real-time) offset))

  (defun reset-time ()
    (setf offset (get-internal-real-time))))
