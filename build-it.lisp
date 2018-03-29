;; usage at shell prompt:
;;
;; cd ~/myproject
;; sbcl --load "build-it.lisp" --name myproject

;;----------------------------------------------------
;; Setup

#-quicklisp (load #P"~/quicklisp/setup.lisp")
(push (merge-pathnames "lib/" *default-pathname-defaults*) asdf:*central-registry*)

#+sbcl
(progn
  #+unix(require 'sb-posix)
  (setf sb-impl::*default-external-format* :utf-8))

;;----------------------------------------------------
;; Load Shipshape

(asdf:load-system :shipshape :force t)

;;----------------------------------------------------
;; Do it

(shipshape::command-line-invoke)
