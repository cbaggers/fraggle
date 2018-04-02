(in-package :fraggle)

(defvar *ssbo* nil)
(defvar *img* nil)

(defun-g fraggle-quad-vert ((pos :vec2))
  (values (v! pos 0 1)
          (* (+ pos (v2! 1f0)) 0.5)))


(defstruct-g picked
  (col :vec4)
  (avg :vec4))


(defun-g fraggle-quad-frag ((uv :vec2)
                            &uniform (now :float)
                            (mouse :vec2)
                            (mouse-norm :vec2)
                            (mouse-buttons :vec2)
                            (screen-res :vec2)
                            (img :sampler-2d)
                            (ssbo picked :ssbo))
  (let* ((half-res (/ screen-res 2))

         (suv (* (v! 1 -1) uv))
         (snext (/ screen-res))
         (mpos (- mouse half-res))
         (mpos-norm mouse-norm)
         (spos (* (vec2 1 -1) mpos-norm))
         (col (texture img spos))
         (avg (/ (+ (texture img (+ spos (* (vec2 -1  1) snext)))
                    (texture img (+ spos (* (vec2  0  1) snext)))
                    (texture img (+ spos (* (vec2  1  1) snext)))
                    (texture img (+ spos (* (vec2 -1  0) snext)))
                    col
                    (texture img (+ spos (* (vec2  1  0) snext)))
                    (texture img (+ spos (* (vec2 -1 -1) snext)))
                    (texture img (+ spos (* (vec2  0 -1) snext)))
                    (texture img (+ spos (* (vec2  1 -1) snext))))
                 9.0))

         (p (- (s~ gl-frag-coord :xy) half-res))
         (dist (sdf:semicircle (sdf:translate p mpos) 50 (radians 100) 15))
         (dist2 (sdf:semicircle (sdf:translate p mpos) 80 (radians 300) 15)))
    (setf (picked-col ssbo) col)
    (setf (picked-avg ssbo) col)
    (mix (mix (mix (mix (texture img suv)
                        col
                        (sdf:mask-fill dist))
                   (v! 0.3 0.3 0.3 0)
                   (sdf:mask-border-inner dist 2))
              avg
              (sdf:mask-fill dist2))
         (v! 0.3 0.3 0.3 0)
         (sdf:mask-border-inner dist2 2))))

(defpipeline-g draw-fraggle ()
  :vertex (fraggle-quad-vert :vec2)
  :fragment (fraggle-quad-frag :vec2))


(defun step-fraggle ()
  (as-frame
    (map-g #'draw-fraggle (get-quad-stream-v2)
           :now (* (now) 0.001)
           :mouse (get-mouse)
           :mouse-norm (get-mouse-norm)
           :mouse-buttons (get-mouse-buttons)
           :screen-res (viewport-resolution (current-viewport))
           :img *img*
           :ssbo *ssbo*)
    (when (mouse-down-p mouse.left)
      (let ((frame (frame-at-point (mouse-pos (mouse)))))
        (when (not (eq frame *last-at-point*))
          (print frame)
          (setf *last-at-point* frame))))
    (clear)
    (tvp-draw)))


(def-simple-main-loop fraggle
    (:on-start #'ensure-fraggle-initialized)
  (step-fraggle))

;;------------------------------------------------------------

(defvar *default-root* nil)
(defvar *targets* nil)

(defun tvp-init ()
  (setf *default-root* (make-frame-root)))

(defun make-frame-root ()
  (let ((root (make-instance 'frame-root)))
    (setf (frame root) (make-frame root))
    root))

(defun make-frame (parent &optional child)
  (check-type child (or null frame-child))
  (let ((frame (make-instance 'frame :parent parent)))
    (with-slots ((frame-child child)) frame
      (setf frame-child
            (or child (make-default-child))))
    frame))

(defun make-default-child ()
  (make-color-target))

(defun register-target (target)
  (check-type target target)
  (push target *targets*)
  target)

;; this the gonna be the screen or some plain or whatever
;; it recieves the layout change info an propegates. Its the
;; only thign with a concrete (non relative) size
(defclass frame-root ()
  ((frame :initarg :frame :accessor frame)))

;; Fills whatever it is in an can hold a single child
(defclass frame ()
  ((parent :initarg :parent :accessor parent)
   (child :initarg :child :accessor child)))

;; the child of a frame can be a split or a target
(defclass frame-child () ())

;; a split can only hold 'split-child'ren, those contain
;; the info on how the space is divided and *must* hold
;; a frame. A split *must* have at least 2 children.
(defclass split (frame-child)
  ((parent :initarg :parent :accessor parent)
   (children :initarg :children :accessor children)))
(defclass vsplit (split) ())
(defclass hsplit (split) ())
(defclass split-child ()
  ((parent :initarg :parent :accessor parent)
   (size :initarg :size :accessor size)
   (frame :initarg :frame :accessor frame)))

;; The only thing that can contain graphics. If you split this
;; it walks up to the nearest split and adds a new split-child.
;; if it hits a frame first then it replaces that frames child
;; with a split and makes itself one of the split's children
;; (by wrapping itself in a split-child)
;;
;; Users can only interact with targets
(defclass target (frame-child) ())

(defclass sampler-target (target)
  ((sampler :initarg :sampler :accessor sampler)))

(defclass color-target (target)
  ((color :initarg :color :accessor color)))

(defpipeline-g color-target-pipeline ()
  (lambda-g ((vert :vec2))
    (v! vert 0 1))
  (lambda-g (&uniform (color3 :vec3))
    color3))

(defvar *default-color-cycle* 0)
(defun make-color-target (&optional color)
  (check-type color (or null vec3))
  (let ((cols nineveh.color::*boytons-11-rarely-confused-colors*))
    (register-target
     (make-instance
      'color-target
      :color (or color
                 (elt cols
                      (setf *default-color-cycle*
                            (mod (1+ *default-color-cycle*)
                                 (length cols)))))))))

;;------------------------------------------------------------

(defvar *last-at-point* nil)

(defun %splits-child-at-point (pos2 viewport self
                               get set new)
  (let ((v (funcall get pos2))
        (dim (funcall get (viewport-resolution viewport)))
        (new-orig (viewport-origin viewport)))
    (loop
       :for split-child :in (children self)
       :for size := (slot-value split-child 'size)
       :for point-size := (* dim size)
       :when (< v point-size)
       :do
       (return (values split-child
                       (make-viewport
                        (funcall new (viewport-resolution viewport)
                                 point-size)
                        new-orig)
                       (funcall new pos2 v)))
       :else :do
       (funcall set new-orig (+ (funcall get new-orig) point-size))
       (decf v point-size)
       :finally (error "Not in viewport ~a ~a" pos2 viewport))))

(defgeneric schild-at-point (pos2 viewport thing)
  (:method (pos2 viewport (self vsplit))
    (flet ((getv (v2) (y v2))
           (setv (v2 val) (setf (y v2) val))
           (newv (v2 val) (v! (x v2) val)))
      (%splits-child-at-point pos2 viewport self #'getv #'setv #'newv)))
  (:method (pos2 viewport (self hsplit))
    (flet ((getv (v2) (x v2))
           (setv (v2 val) (setf (x v2) val))
           (newv (v2 val) (v! val (y v2))))
      (%splits-child-at-point pos2 viewport self #'getv #'setv #'newv))))

(defun %frame-at-point (pos2 viewport thing)
  ;; assumes pos2 is within viewport
  (etypecase thing
    (frame-root (%frame-at-point pos2 viewport (frame thing)))
    (frame (with-slots (child) thing
             (etypecase child
               (split (%frame-at-point pos2 viewport child))
               (target thing))))
    (split
     (multiple-value-bind (split-child new-viewport new-pos2)
         (schild-at-point pos2 viewport thing)
       (%frame-at-point new-pos2 new-viewport (frame split-child))))))

(defun frame-at-point (pos2)
  (let* ((viewport (current-viewport))
         (ox (viewport-origin-x viewport))
         (oy (viewport-origin-y viewport))
         (pos2 (v! (clamp ox
                          (+ ox (viewport-resolution-x viewport))
                          (x pos2))
                   (clamp oy
                          (+ oy (viewport-resolution-y viewport))
                          (y pos2))))
         (result (%frame-at-point pos2 viewport *default-root*)))
    result))

;;------------------------------------------------------------

(defun switch-to-target (frame target)
  (check-type frame frame)
  (check-type target target)
  (with-slots (child) frame
    (assert (typep child 'target) ()
            "Cannot switch frame to hold ~a as it holds the split ~a"
            target child)
    (setf child target)))

;;------------------------------------------------------------

(defun find-compatible-split (thing split-type)
  (etypecase thing
    (split (when (typep thing split-type)
             thing))
    (frame nil)
    (frame-root nil)
    (t (find-compatible-split (parent thing) split-type))))

(defun split-exisiting (frame split)
  (let* ((schild (parent frame))
         (size (size schild))
         (new-size (* size 0.5))
         (child-pos (position schild (children split)))
         (new-frame (make-instance 'frame :child (child frame)))
         (new-child (make-instance 'split-child
                                   :parent split
                                   :size new-size
                                   :frame new-frame)))
    (assert child-pos () "BUG")
    (setf (parent new-frame) new-child)
    (setf (size schild) new-size)
    (setf (children split)
          (append (subseq (children split) 0 (1+ child-pos))
                  (list new-child)
                  (subseq (children split) (1+ child-pos))))
    frame))

(defun fresh-split (frame split-type)
  (check-type frame frame)
  (check-type (child frame) target)
  (let* ((target (child frame))
         ;; we need at least 2 children for a split
         ;; so we make 2 frames that both hold the same
         ;; target. Do not share frames!
         (split (make-instance split-type :parent frame))
         (new-frame0 (make-instance 'frame :child target))
         (schild0 (make-instance 'split-child
                                 :parent split
                                 :size 0.5
                                 :frame new-frame0))
         (new-frame1 (make-instance 'frame :child target))
         (schild1 (make-instance 'split-child
                                 :parent split
                                 :size 0.5
                                 :frame new-frame1)))
    (setf (parent new-frame0) schild0
          (parent new-frame1) schild1
          (children split) (list schild0 schild1)
          (child frame) split)
    new-frame0))

(defun %split (frame kind)
  (check-type frame frame)
  (let* ((split (find-compatible-split (parent frame) kind))
         (new-frame (if split
                        (split-exisiting frame split)
                        (fresh-split frame kind))))
    (when (eq frame *last-at-point*)
      (setf *last-at-point* new-frame)))
  frame)

(defun split-vertically (frame)
  (%split frame 'vsplit))

(defun split-horizontally (frame)
  (%split frame 'hsplit))

;;------------------------------------------------------------

(defun %remove-frame (split frame)
  (let* ((schild (parent frame))
         (children (children split))
         (child-pos (position schild children))
         (new-children (remove schild children)))
    (assert child-pos ()
            "The frame ~a is not a decendent of ~a" frame split)
    (if (= (length new-children) 1)
        (let ((new-child (child (frame (first new-children))))
              (new-parent (parent split)))
          (check-type new-parent frame)
          (when (typep new-child 'split)
            (print "yay!")
            (setf (parent new-child) new-parent))
          (setf (child new-parent) new-child)
          new-child)
        (progn
          (setf (children split) new-children)
          (loop :for c :in new-children :do
             (incf (size c) (/ (size schild) (length new-children))))
          (frame (elt new-children (max 0 (1- child-pos))))))))

(defun pop-frame (frame)
  (check-type frame frame)
  (let ((split (find-compatible-split (parent frame) 'split)))
    (if split
        (%remove-frame split frame)
        (warn "Cannot pop this frame as it is the only one."))))

;;------------------------------------------------------------

;; internal only, shouldnt be extended
(defgeneric draw (thing new-viewport))

(defmethod draw ((this frame-root) new-viewport)
  (with-slots (frame) this
    (draw frame new-viewport)))

(defmethod draw ((this frame) new-viewport)
  (with-slots (child) this
    (draw child new-viewport)))

(defmethod draw ((this vsplit) new-viewport)
  (with-slots (children) this
    (let* ((avaliable-size 1.0)
           (orig-height (viewport-resolution-y new-viewport))
           (rx (viewport-resolution-x new-viewport))
           (ox (viewport-origin-x new-viewport))
           (oy (+ (viewport-origin-y new-viewport) orig-height)))
      (loop :for split-child :in children :do
         (with-slots (size) split-child
           (let* ((point-size (* size orig-height))
                  (new-oy (- oy point-size)))
             (draw (frame split-child)
                   (make-viewport
                    (list rx point-size)
                    (list ox new-oy)))
             (setf oy new-oy)
             (setf avaliable-size (max 0 (- avaliable-size size)))))))))

(defmethod draw ((this hsplit) new-viewport)
  (with-slots (children) this
    (let* ((avaliable-size 1.0)
           (orig-width (viewport-resolution-x new-viewport))
           (ry (viewport-resolution-y new-viewport))
           (o (viewport-origin new-viewport)))
      (loop :for split-child :in children :do
         (with-slots (size) split-child
           (let* ((point-size (* size orig-width)))
             (draw (frame split-child)
                   (make-viewport (vec2 point-size ry) o))
             (incf (x o) point-size)
             (setf avaliable-size (max 0 (- avaliable-size size)))))))))

(defmethod draw ((this color-target) new-viewport)
  (with-viewport new-viewport
    (with-slots (color) this
      (map-g #'color-target-pipeline (nineveh:get-quad-stream-v2)
             :color3 color))))
;;------------------------------------------------------------

(defun tvp-draw ()
  (draw *default-root* (current-viewport)))
