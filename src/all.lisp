(uiop:define-package solar-system/all
  (:use :cl)
  (:use-reexport
   #:solar-system/camera
   #:solar-system/core
   #:solar-system/cubemap
   #:solar-system/shaders
   #:solar-system/sphere))
