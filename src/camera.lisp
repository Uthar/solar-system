(defpackage solar-system/camera
  (:use :cl :3d-vectors :3d-matrices)
  (:export
   #:rad
   #:deg
   #:camera
   #:view-matrix
   #:look-around
   #:walk))

(in-package solar-system/camera)

(declaim (inline rad))
(defun rad (degree)
  (coerce (* degree (/ pi 180)) 'single-float))

(declaim (inline deg))
(defun deg (radian)
  (coerce (* radian (/ 180 pi)) 'single-float))

(defclass camera ()
  ((position  :initform (vec3 0 0 3) :initarg :position)
   (direction :initform (vec3 0 0 -1))
   (zoom  :initform 45.0)
   (yaw   :initform -90.0)
   (pitch :initform 0.0)))

(defmethod initialize-instance :after ((camera camera) &key)
  (with-slots (direction yaw pitch) camera
    (setf direction (direction-vector yaw pitch))))

(defun direction-vector (yaw pitch)
  (let ((x (* (cos (rad yaw)) (cos (rad pitch))))
        (y (sin (rad pitch)))
        (z (* (sin (rad yaw)) (cos (rad pitch)))))
    (v- (vunit (vec3 x y z)))))

(defun camera (x y z)
  (make-instance 'camera :position (vec3 x y z)))

(defmethod view-matrix ((camera camera))
  (with-slots (position direction) camera
    (3d-matrices:mlookat position (v- position direction) +vy+)))

(defmethod look-around ((camera camera) x-offset y-offset)
  (with-slots (yaw pitch direction) camera
    (setf yaw (+ yaw x-offset)
          pitch (clamp (- pitch y-offset) -89.0 89.0)
          direction (direction-vector yaw pitch))
    (view-matrix camera)))

(defmethod walk ((camera camera) &key (x 0.0) (y 0.0) (z 0.0))
  (with-slots (position direction) camera
    (nv+ position (v* (vunit (vc direction +vy+)) x))
    (nv+ position (v* (v- (vc direction (vunit (vc direction +vy+)))) y))
    (nv+ position (v* direction z)))
  (view-matrix camera))
