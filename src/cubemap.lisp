(defpackage solar-system/cubemap
  (:use :cl)
  (:local-nicknames
   (:png :pngload)))

(in-package solar-system/cubemap)

(defclass cubemap ()
  ((texture :initform 0)
   (negx :initarg :negx :initform (error "negx required"))
   (negy :initarg :negy :initform (error "negy required"))
   (negz :initarg :negz :initform (error "negz required"))
   (posx :initarg :posx :initform (error "posx required"))
   (posy :initarg :posy :initform (error "posy required"))
   (posz :initarg :posz :initform (error "posz required"))))

(defmethod initialize-instance :after ((cubemap cubemap) &key)
  (with-slots (negx negy negz posx posy posz)
      cubemap
    (let ((texture (gl:gen-texture)))
      (gl:bind-texture :texture-cube-map texture)
      (flet ((load-part (target file)
               (let ((png (png:load-file file :flatten t)))
                 (gl:tex-image-2d target
                                  0
                                  4
                                  (png:width png)
                                  (png:height png)
                                  0
                                  :rgb
                                  :unsigned-byte
                                  (png:data png)))))
        (load-part :texture-cube-map-positive-x posx)
        (load-part :texture-cube-map-positive-y posy)
        (load-part :texture-cube-map-positive-z posz)
        (load-part :texture-cube-map-negative-x negx)
        (load-part :texture-cube-map-negative-y negy)
        (load-part :texture-cube-map-negative-z negz))
      (gl:tex-parameter :texture-cube-map :texture-mag-filter :linear)
      (gl:tex-parameter :texture-cube-map :texture-min-filter :linear)
      (gl:tex-parameter :texture-cube-map :texture-wrap-s :clamp-to-edge)
      (gl:tex-parameter :texture-cube-map :texture-wrap-t :clamp-to-edge)
      (gl:tex-parameter :texture-cube-map :texture-wrap-r :clamp-to-edge)
      (gl:bind-texture :texture-cube-map 0)
      (setf (slot-value cubemap 'texture) texture))))
