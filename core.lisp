(defpackage solar-system/core
  (:use :cl)
  (:local-nicknames
   (:a :alexandria)
   (:m :3d-matrices)
   (:v :3d-vectors)
   (:shaders :solar-system/shaders)
   (:sphere :solar-system/sphere)
   (:camera :solar-system/camera)
   (:cubemap :solar-system/cubemap)))

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

(let ((start (sb-ext:get-time-of-day)))
  (defun get-time ()
    (multiple-value-bind (s us)
        (sb-ext:get-time-of-day)
      (- (+ s (/ us 1000000.0d0)) start))))

(defun render ()
  (gl:viewport 0 0 500 500)
  (gl:clear-color 0.2 0.4 0.5 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:use-program (slot-value *shader* 'shaders::program))
  (gl:bind-texture :texture-cube-map (slot-value *cubemap* 'cubemap::texture))
  (gl:uniform-matrix-4fv (gl:get-uniform-location
                          (slot-value *shader* 'shaders::program)
                          "model")
                         (m:marr (m:mrotation v:+vy+ (let ((time (get-time)))
                                                       (* 30
                                                          (camera::rad time))))))
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
  (gl:polygon-mode :front-and-back :fill)
  (gl:draw-arrays :triangles 0 (* 3 (length (sphere:make-sphere 5))))
  (gl:bind-vertex-array *sphere*)
  (gl:bind-texture :texture-cube-map 0)
  (gl:use-program 0)
  (sdl2:gl-swap-window *window*))

(defvar *sphere* nil)
(defvar *camera* nil)
(defvar *shader* nil)
(defvar *cubemap* nil)

(defun init ()
  (setf *window* (open-window 500 500))
  (gl:enable :depth-test)
  (setf *sphere* (sphere:load-sphere (sphere:make-sphere 5)))
  (setf *camera* (camera:camera 0 0 3))
  (setf *shader* (make-instance 'shaders:opengl-shader
                                :vert "glsl/model-view-projection.vert"
                                :frag "glsl/cubemap.frag"))
  (setf *cubemap* (make-instance 'cubemap::cubemap
                                 :posx "./posx.png"
                                 :posy "./posy.png"
                                 :posz "./posz.png"
                                 :negx "./negx.png"
                                 :negy "./negy.png"
                                 :negz "./negz.png")))

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
