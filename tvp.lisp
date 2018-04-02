(in-package :fraggle)

;;------------------------------------------------------------
;; Types

;; The extensible part of the system. This gets attached to
;; a frame which then means it's render function will get
;; called.
;; If you split this it walks up to the nearest split and adds a
;; new split-child. if it hits a frame first then it replaces that
;; frames child with a split and makes itself one of the split's
;; children (by wrapping itself in a split-child)
(defclass target ()
  ((name :initform nil :initarg :name :reader name)))

(deftype frame-child ()
  `(or split target))

;; Fills whatever it is in an can hold a single child
(defstruct (frame (:constructor %make-frame))
  (parent nil :type (or null frame-holder))
  (child (error "BUG: Frame with no child")
         :type frame-child))

;; only tvp-root will allow nil frame
(defstruct (frame-holder (:constructor nil))
  (frame nil :type (or null frame))
  (viewport (make-viewport) :type viewport))

;; a split can only hold 'split-child'ren, those contain
;; the info on how the space is divided and *must* hold
;; a frame. A split *must* have at least 2 children.

(defstruct
    (split-child
      (:include frame-holder
                (frame (error "Bug: Split child with no frame")
                       :type frame)))
  (parent (error "Bug: Split child with no parent"))
  (size 1.0 :type single-float))



(defstruct split
  (parent (error "Bug: Split with no parent")
          :type frame)
  (children (make-split-array)
            :type (array split-child (*))))
(defstruct (vsplit (:include split)))
(defstruct (hsplit (:include split)))


;; relies on the system to populate the frame on creation
(defstruct (tvp-root (:include frame-holder)))

(defstruct (tvp-system (:constructor %make-tvp-system))
  (root (error "Bug: tvp-system without a root") :type tvp-root)
  (targets (make-hash-table :test #'eq)
           :type hash-table)
  (draw-array (make-draw-array)
              :type (array (or null draw-pair) (*))))

(defstruct draw-pair
  (target (error "Bug: draw-pair without target") :type target)
  (viewport (error "Bug: draw-pair without viewport") :type viewport))

;;------------------------------------------------------------
;; Globals

(declaim (type (or null tvp-system) *default-tvp-system*))
(defvar *default-tvp-system* nil)

(defvar *last-at-point* nil) ;; horrible hack, replace

;;------------------------------------------------------------
;; Frame

(defun make-frame (child &optional parent)
  (check-type child frame-child)
  (check-type parent (or null frame-holder))
  (%make-frame
   :child child
   :parent parent))

(defmethod print-object ((obj frame) stream)
  (print-unreadable-object (obj stream :type T)
    (format stream "~@[:CHILD ~a~]" (frame-child obj)))
  obj)

;;------------------------------------------------------------
;; System

;; {TODO} put flag on system so we can say it owns a whole surface
;;        then we can do full recompute on every frame yada yada

(defun make-tvp-system ()
  (let* ((root (make-tvp-root))
         (name :scratch)
         (target (%make-default-target name))
         (frame (make-frame target root)))
    (setf (tvp-root-frame root) frame)
    (let ((sys (%make-tvp-system :root root)))
      (setf (gethash name (tvp-system-targets sys))
            target)
      sys)))

;; replace this with cepl style current system stuff
(defun default-tvp-system ()
  (or *default-tvp-system*
      (setf *default-tvp-system*
            (make-tvp-system))))


(defun tvp-reinit (&optional (system (default-tvp-system)))
  (let* ((root (make-tvp-root))
         (name :scratch)
         (target (%make-default-target name))
         (frame (make-frame target root)))
    (setf (tvp-root-frame root) frame
          (tvp-system-root system) root
          (tvp-system-targets system) (make-hash-table :test #'eq))
    (setf (gethash name (tvp-system-targets system))
          target)
    system))

(defmethod print-object ((obj tvp-root) stream)
  (print-unreadable-object (obj stream :type T :identity t))
  obj)

(defun make-draw-array ()
  (make-array 30 :element-type '(or null draw-pair)
              :fill-pointer 0
              :adjustable t
              :initial-element nil))

(defun reset-draw-array (system)
  (let ((arr (tvp-system-draw-array system)))
    (loop :for i :below (length arr) :do
       (setf (aref arr i) nil)
       (setf (fill-pointer arr) 0)))
  system)

(defun tvp-draw (&optional (system (default-tvp-system)))
  (let ((arr (tvp-system-draw-array system)))
    (loop :for draw-pair :across arr :do
       (with-viewport (draw-pair-viewport draw-pair)
         (draw (draw-pair-target draw-pair))))))

(defun tvp-layout (&optional
                     (system (default-tvp-system))
                     (recalc-draw-list t))
  (let ((arr (tvp-system-draw-array system)))
    (flet ((enqueue (x)
             (vector-push-extend x arr)))
      (let ((enq (when recalc-draw-list
                   (reset-draw-array system)
                   #'enqueue)))
        (layout (tvp-system-root system)
                (current-viewport)
                enq)))))

;;------------------------------------------------------------
;; Targets

(defmethod initialize-instance :after ((inst target) &key name)
  (assert (symbolp name) ()
          "Target names must be symbols. Found ~s"
          name))

(defun target (name &optional (system (default-tvp-system)))
  (values (gethash name (tvp-system-targets system))))

(defun target-names (&optional (system (default-tvp-system)))
  (let (names)
    (maphash
     (lambda (k v)
       (declare (ignore v))
       (push k names))
     (tvp-system-targets system))
    names))

(defun register-target (target &optional (system (default-tvp-system)))
  (check-type target target)
  (assert (name target) ()
          "Cannot use a target without a name: ~a"
          target)
  (let ((existing (target (name target) system)))
    (unless (eq existing target)
      (assert (not existing) ()
              "Target named '~a' already exists in system. Cannot add ~a"
              (name target) target)
      (setf (gethash (name target) (tvp-system-targets system))
            target)))
  target)

(defmethod print-object ((obj target) stream)
  (print-unreadable-object (obj stream :type T)
    (with-slots (name) obj
      (format stream "~@[:NAME ~s~]" name)))
  obj)

;;------------------------------------------------------------
;; Splits


(defun make-split-array (&optional initial-contents)
  (make-array (length initial-contents)
              :initial-contents initial-contents
              :element-type 'split-child
              :fill-pointer (length initial-contents)
              :adjustable t))

(defun insert-into-split-array (arr pos item)
  (let ((len (length arr)))
    (vector-push-extend (aref arr (- len 1)) arr 4)
    (loop :for i :from pos :below len :do
       (setf (aref arr i) (aref arr (- i 1))))
    (setf (aref arr pos) item)
    item))

(defun remove-split-child (arr pos)
  (loop :for i :from pos :below (- (length arr) 1) :do
     (setf (aref arr i) (aref arr (1+ i))))
  (decf (fill-pointer arr))
  arr)

(defmethod print-object ((obj split-child) stream)
  (print-unreadable-object (obj stream :type T)
    (format stream "~@[:FRAME ~a~]" (split-child-frame obj)))
  obj)

(defmethod print-object ((obj split) stream)
  (print-unreadable-object (obj stream :type T)
    (format stream "~@[:CHILDREN ~a~]"
            (split-children obj)))
  obj)

;;------------------------------------------------------------

(defun %splits-child-at-point (pos2 viewport self get set new)
  (declare (type split self)
           (type vec2 pos2)
           (type viewport viewport)
           (type (function (vec2) single-float) get)
           (type (function (vec2 single-float) single-float) set)
           (type (function (vec2 single-float) vec2) new))
  ;;
  (let ((v (funcall get pos2))
        (dim (funcall get (viewport-resolution viewport)))
        (new-orig (viewport-origin viewport)))
    (loop
       :for split-child :across (split-children self)
       :for size := (split-child-size split-child)
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

(defun schild-at-point (pos2 viewport thing)
  (etypecase thing
    (vsplit
     (flet ((getv (v2) (y v2))
            (setv (v2 val) (setf (y v2) val))
            (newv (v2 val) (v! (x v2) val)))
       (%splits-child-at-point pos2 viewport thing #'getv #'setv #'newv)))
    (hsplit
     (flet ((getv (v2) (x v2))
            (setv (v2 val) (setf (x v2) val))
            (newv (v2 val) (v! val (y v2))))
       (%splits-child-at-point pos2 viewport thing #'getv #'setv #'newv)))))

(defun %frame-at-point (pos2 viewport thing)
  ;; assumes pos2 is within viewport
  (etypecase thing
    (tvp-root (%frame-at-point pos2 viewport (tvp-root-frame thing)))
    (frame (let ((child (frame-child thing)))
             (etypecase child
               (split (%frame-at-point pos2 viewport child))
               (target thing))))
    (split
     (multiple-value-bind (split-child new-viewport new-pos2)
         (schild-at-point pos2 viewport thing)
       (%frame-at-point new-pos2
                        new-viewport
                        (split-child-frame split-child))))))

;; {TODO} base on system
(defun frame-at-point (pos2 &optional (system (default-tvp-system)))
  (let* ((viewport (current-viewport))
         (ox (viewport-origin-x viewport))
         (oy (viewport-origin-y viewport))
         (pos2 (v! (clamp ox
                          (+ ox (viewport-resolution-x viewport))
                          (x pos2))
                   (clamp oy
                          (+ oy (viewport-resolution-y viewport))
                          (y pos2)))))
    (%frame-at-point pos2 viewport (tvp-system-root system))))

;;------------------------------------------------------------

(defun switch-to-target (frame target)
  (check-type frame frame)
  (check-type target target)
  (let ((child (frame-child frame)))
    (assert (typep child 'target) ()
            "Cannot switch frame to hold ~a as it holds the split ~a"
            target child)
    (setf (frame-child frame) target)))

;;------------------------------------------------------------

(declaim (ftype (function (t (function (t) boolean))
                          (or null split))
                find-compatible-split))
(defun find-compatible-split (thing predicate)
  (print thing)
  (cond
    ((funcall predicate thing) thing)
    ((split-child-p thing)
     (find-compatible-split (split-child-parent thing) predicate))))

(declaim (ftype (function (frame split) frame) split-exisiting))
(defun split-exisiting (frame split)
  (let* ((schild (frame-parent frame))
         (size (split-child-size schild))
         (new-size (* size 0.5))
         (child-pos (position schild (split-children split)))
         (new-frame (make-frame (frame-child frame)))
         (new-child (make-split-child :parent split
                                      :frame new-frame
                                      :size new-size)))
    (assert child-pos () "Bug: Child missing from parent. Tree broken.")
    (setf (frame-parent new-frame) new-child)
    (setf (split-child-size schild) new-size)
    (insert-into-split-array (split-children split) (1+ child-pos) new-child)
    frame))

(defun fresh-split (frame predicate)
  (check-type frame frame)
  ;; ↓ as otherwise we are splitting a non-leaf frame
  (check-type (frame-child frame) target)
  (let* ((target (frame-child frame))
         ;; we need at least 2 children for a split
         ;; so we make 2 frames that both hold the same
         ;; target. Do not share frames!
         (split (cond
                  ((eq predicate #'vsplit-p) (make-vsplit :parent frame))
                  ((eq predicate #'hsplit-p) (make-hsplit :parent frame))
                  (t (error "Bug: invalid split predicate"))))
         (new-frame0 (make-frame target))
         (new-frame1 (make-frame target))
         (schild0 (make-split-child :parent split
                                    :frame new-frame0
                                    :size 0.5))
         (schild1 (make-split-child :parent split
                                    :frame new-frame1
                                    :size 0.5)))
    (setf (frame-parent new-frame0) schild0
          (frame-parent new-frame1) schild1
          (split-children split) (make-split-array (list schild0 schild1))
          (frame-child frame) split)
    new-frame0))

(defun %split (frame predicate)
  (check-type frame frame)
  (assert (or (eq predicate #'vsplit-p) (eq predicate #'hsplit-p)))
  (let* ((split (find-compatible-split (frame-parent frame) predicate))
         (new-frame (if split
                        (split-exisiting frame split)
                        (fresh-split frame predicate))))
    (when (eq frame *last-at-point*)
      (setf *last-at-point* new-frame)))
  frame)

(defun split-vertically (frame)
  (%split frame #'vsplit-p))

(defun split-horizontally (frame)
  (%split frame #'hsplit-p))

;;------------------------------------------------------------

(defun %remove-frame (split frame)
  (let* ((schild (frame-parent frame))
         (child-pos (position schild (split-children split))))
    (assert child-pos ()
            "The frame ~a is not a decendent of ~a" frame split)
    ;; split has to have at least 2 children
    (if (= (length (split-children split)) 2)
        ;; remove the split
        (let ((new-child (frame-child
                          (split-child-frame
                           (if (= child-pos 0)
                               (elt (split-children split) 1)
                               (elt (split-children split) 0)))))
              (new-parent (split-parent split)))
          (check-type new-parent frame)
          (when (split-p new-child)
            (setf (split-parent new-child) new-parent))
          (setf (frame-child new-parent) new-child)
          new-child)
        ;; keep the split
        (progn
          (remove-split-child (split-children split) child-pos)
          (let ((size (/ (split-child-size schild)
                         (length (split-children split)))))
            (loop :for c :across (split-children split) :do
               (incf (split-child-size c) size)))
          (split-child-frame (elt (split-children split)
                                  (max 0 (1- child-pos))))))))

(defun pop-frame (frame)
  (check-type frame frame)
  (let ((split (find-compatible-split (frame-parent frame) #'split-p)))
    (if split
        (%remove-frame split frame)
        (warn "Cannot pop this frame as it is the only one."))))

;;------------------------------------------------------------
;; Layout

(defgeneric layout (thing new-viewport enqueue))

(defmethod layout ((this tvp-root) new-viewport enqueue)
  (layout (tvp-root-frame this) new-viewport enqueue))

(defmethod layout ((this frame) new-viewport enqueue)
  (if (typep (frame-child this) 'target)
      (when enqueue
        (funcall enqueue
                 (make-draw-pair
                  :target (frame-child this)
                  :viewport new-viewport)))
      (layout (frame-child this) new-viewport enqueue)))

(defmethod layout ((this vsplit) new-viewport enqueue)
  (let* ((avaliable-size 1.0)
         (orig-height (viewport-resolution-y new-viewport))
         (rx (viewport-resolution-x new-viewport))
         (ox (viewport-origin-x new-viewport))
         (oy (+ (viewport-origin-y new-viewport) orig-height)))
    (loop :for split-child :across (vsplit-children this) :do
       (let* ((size (split-child-size split-child))
              (point-size (* size orig-height))
              (new-oy (- oy point-size))
              (vp (make-viewport
                   (list rx point-size)
                   (list ox new-oy))))
         (setf (split-child-viewport split-child) vp)
         (layout (split-child-frame split-child) vp enqueue)
         (setf oy new-oy)
         (setf avaliable-size (max 0 (- avaliable-size size)))))))

(defmethod layout ((this hsplit) new-viewport enqueue)
  (let* ((avaliable-size 1.0)
         (orig-width (viewport-resolution-x new-viewport))
         (ry (viewport-resolution-y new-viewport))
         (o (viewport-origin new-viewport)))
    (loop :for split-child :across (hsplit-children this) :do
       (let* ((size (split-child-size split-child))
              (point-size (* size orig-width))
              (vp (make-viewport (vec2 point-size ry) o)))
         (setf (split-child-viewport split-child) vp)
         (layout (split-child-frame split-child) vp enqueue)
         (incf (x o) point-size)
         (setf avaliable-size (max 0 (- avaliable-size size)))))))

;;------------------------------------------------------------
;; Draw

(defgeneric draw (target))

;;------------------------------------------------------------
;; Color Target

(defclass color-target (target)
  ((color :initarg :color :accessor color)))

(defpipeline-g color-target-pipeline ()
  (lambda-g ((vert :vec2))
    (v! vert 0 1))
  (lambda-g (&uniform (color3 :vec3))
    color3))

(defvar *default-color-cycle* 0)
(defun %make-color-target (name color)
  (check-type color (or null vec3))
  (let ((cols nineveh.color::*boytons-11-rarely-confused-colors*))
    (make-instance
     'color-target
     :color (or color
                (elt cols
                     (setf *default-color-cycle*
                           (mod (1+ *default-color-cycle*)
                                (length cols)))))
     :name name)))

(defun make-color-target (&key name color (system (default-tvp-system)))
  (check-type color (or null vec3))
  (register-target (%make-color-target name color) system))

(defun %make-default-target (name)
  (%make-color-target name (v! 0.03 0.03 0.05)))

(defmethod draw ((this color-target))
  (with-slots (color) this
    (map-g #'color-target-pipeline (nineveh:get-quad-stream-v2)
           :color3 color)))
