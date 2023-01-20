(defpackage solar-system/shaders
  (:use :cl)
  (:import-from
   :alexandria
   :read-file-into-string
   :emptyp)
  (:export
   #:opengl-shader))

(in-package solar-system/shaders)

(defun compile-shader (type path)
  (let ((src (read-file-into-string path))
        (id (gl:create-shader type)))
    (assert (plusp id) (id) "Cannot create GL shader")
    (gl:shader-source id src)
    (gl:compile-shader id)
    (let ((log (gl:get-shader-info-log id)))
      (unless (gl:get-shader id :compile-status)
        (error "Error compiling shader: ~a" log))
      (unless (emptyp log)
        (warn "During compiling of shader ~a: ~a" path log)))
    id))

(defun link-shader-program (vs fs)
  (let ((id (gl:create-program)))
    (assert (plusp id) (id) "Cannot create GL shader program")
    (gl:attach-shader id vs)
    (gl:attach-shader id fs)
    (gl:link-program id)
    (let ((log (gl:get-program-info-log id)))
      (unless (gl:get-program id :link-status)
        (error "Error linking shader program: ~a" log))
      (unless (emptyp log)
        (warn "During compiling of shader program: ~a" log)))
    id))

(defclass opengl-shader ()
  ((program :initarg :program :initform 0)
   (vert :initarg :vert :initform (error "path to vertex shader required"))
   (frag :initarg :frag :initform (error "path to fragment shader required"))
   (locations :initform (make-hash-table :test 'equal))))

(defun build-shader-program (&key vert frag)
  (let ((vs nil)
        (fs nil)
        (program nil))
    (unwind-protect
         (setf vs (compile-shader :vertex-shader vert)
               fs (compile-shader :fragment-shader frag)
               program (link-shader-program vs fs))
      (handler-case
          (progn
            (when vs
              (gl:delete-shader vs))
            (when fs
              (gl:delete-shader fs)))
        (error (e)
          (warn "Could not clean up shaders: ~a" e))))))

(defmethod initialize-instance :after ((shader opengl-shader) &key vert frag)
  (with-slots (program)
      shader
    (setf program (build-shader-program :vert vert :frag frag))))
