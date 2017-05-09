;;;; package.lisp

(defpackage #:fraggle
  (:import-from :rtg-math.projection :perspective)
  (:import-from #:cepl.camera
                :export :make-camera :in-space :fov
                :cam->clip :x->cam :using-camera :camera-pos :camera-rot
                :camera-viewport)
  (:use #:cl #:temporal-functions #:cepl #:named-readtables
        #:varjo-lang #:rtg-math :rtg-math.base-maths #:skitter.sdl2.keys
        #:skitter.sdl2.mouse-buttons #:structy-defclass #:nineveh))
