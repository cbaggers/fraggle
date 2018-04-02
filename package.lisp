;;;; package.lisp

(defpackage #:fraggle
  (:import-from :rtg-math.projection :perspective)
  (:import-from #:cepl.camera
                :export :make-camera :in-space :fov
                :cam->clip :x->cam :using-camera :camera-pos :camera-rot
                :camera-viewport)
  (:use #:cl #:temporal-functions #:cepl #:named-readtables
        #:vari #:rtg-math :rtg-math.base-maths
        #:cepl.skitter #:structy-defclass #:nineveh))

(uiop:define-package :sdf
    (:use :nineveh.sdf.2d)
  (:reexport :nineveh.sdf.2d))

(uiop:define-package :tvm
    (:use :tiling-viewport-manager)
  (:reexport :tiling-viewport-manager))
