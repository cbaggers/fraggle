(in-package :fraggle)

;; init

(defvar *initd* nil)
(defvar *sdl2-pads* nil)

(defun init-pads (ids)
  (setf *sdl2-pads*  (make-array 10 :initial-element nil))
  (sdl2-game-controller-db:load-db)
  (loop :for id :in ids :do
     (unless (aref *sdl2-pads* id)
       (setf (aref *sdl2-pads* id)
             (sdl2:game-controller-open id))))
  (skitter.sdl2:enable-background-joystick-events))

(defun ensure-fraggle-initialized ()
  (unless *initd*
    ;; we need an gl-initialized-p function in cepl
    (when (cepl.lifecycle:uninitialized-p)
      (cepl:repl))
    (unless *sdl2-pads*
      (init-pads '(0)))
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
    (float (- (get-internal-real-time) offset) 0f0))

  (defun reset-time ()
    (setf offset (get-internal-real-time))))
