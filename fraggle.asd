;;;; fraggle.asd

(asdf:defsystem #:fraggle
  :description "An experient to find the working state of cepl"
  :author "Baggers <techsnuffle@gmail.com>"
  :license "GPLv3"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :serial t
  :depends-on (#:cepl.camera
               #:cepl.sdl2
               #:cepl.skitter.sdl2
               #:cl-fad
               #:classimp
               #:dendrite
               #:dirt
               #:disposable
               #:fn
               #:named-readtables
               #:nineveh
               #:structy-defclass
               #:swank.live
               #:temporal-functions
               #:tiling-viewport-manager)
  :components ((:file "package")
               (:file "system")
               (:file "misc-gpu-funcs")
               (:file "main")))
