;;;; fraggle.asd

(asdf:defsystem #:fraggle
  :description "An experient to find the working state of cepl"
  :author "Baggers <techsnuffle@gmail.com>"
  :license "GPLv3"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :serial t
  :depends-on (#:cepl.sdl2 #:cepl.camera #:cepl.skitter.sdl2
                           #:classimp #:fn #:named-readtables #:cl-fad
                           #:temporal-functions #:dendrite #:disposable
                           #:structy-defclass #:swank.live
                           #:dirt #:nineveh)
  :components ((:file "package")
               (:file "system")
               (:file "misc-gpu-funcs")
               (:file "main")
               (:file "tvp")))
