(defpackage solar-system/core
  (:use :cl)
  (:local-nicknames
   (:a :alexandria)
   (:m :3d-matrices)
   (:v :3d-vectors)
   (:shaders :solar-system/shaders)
   (:sphere :solar-system/sphere)
   (:camera :solar-system/camera)))

(in-package solar-system/core)

(defvar *window* nil)
(defvar *opengl-context* nil)

(defun open-window (width height)
  (sdl2:init :everything)
  (setf *window* (sdl2:create-window :w width :h height :flags '(:opengl)))
  (setf *opengl-context* (sdl2:gl-create-context *window*))
  *window*)

(defun destroy-window (&optional (window *window*))
  (sdl2:gl-delete-context *opengl-context*)
  (sdl2:destroy-window *window*)
  (values))

;; (progn
;;   (sdl2:gl-make-current *window* *opengl-context*)
;;   (gl:viewport 0 0 100 50)
;;   (gl:clear-color 0.7 0.4 0.2 1.0)
;;   (gl:clear :color-buffer-bit)
;;   (sdl2:gl-swap-window *window*))

(defun render ()
  (gl:viewport 0 0 500 500)
  (gl:clear-color 0.2 0.4 0.5 1.0)
  (gl:clear :color-buffer-bit)
  (gl:use-program (slot-value *shader* 'shaders::program))
  (gl:uniform-matrix-4fv (gl:get-uniform-location
                          (slot-value *shader* 'shaders::program)
                          "model")
                         (m:marr (m:meye 4)))
  (let ((view (camera:view-matrix *camera*))
        (projection (m:mperspective (slot-value *camera* 'camera::zoom)
                                    (/ 500 500)
                                    0.1 100.0)))
    (gl:uniform-matrix-4fv (gl:get-uniform-location
                            (slot-value *shader* 'shaders::program)
                            "view")
                           (m:marr view))
    (gl:uniform-matrix-4fv (gl:get-uniform-location
                            (slot-value *shader* 'shaders::program)
                            "projection")
                           (m:marr projection)))
  (gl:bind-vertex-array *sphere*)
  (gl:polygon-mode :front-and-back :line)
  (gl:draw-arrays :triangles 0 (* 60 128))
  (gl:bind-vertex-array *sphere*)
  (gl:use-program 0)
  (sdl2:gl-swap-window *window*)
  (sleep 0.16))

(defvar *sphere* nil)
(defvar *camera* nil)
(defvar *shader* nil)

(defun init ()
  (setf *window* (open-window 500 500))
  (setf *sphere* (sphere:load-sphere (sphere:make-sphere 3)))
  (setf *camera* (camera:camera 0 0 3))
  (setf *shader* (make-instance 'shaders:opengl-shader
                                :vert "glsl/model-view-projection.vert"
                                :frag "glsl/one-texture.frag")))

(defun mainloop ()
  ;; (sdl2:gl-make-current *window* *opengl-context*)
  (init)
  (loop
    (render)
    (sleep 0.16)))
  

(defvar *render-thread* nil)

(defun main ()
  (setf *render-thread* (sb-thread:make-thread #'mainloop))
  (sb-thread:join-thread *render-thread*))

;; (sb-thread:destroy-thread *render-thread*)
