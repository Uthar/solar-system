(defsystem solar-system
  :description "Solar system 3D simulation"
  :license "GPLv3"
  :depends-on (alexandria sdl2 cl-opengl 3d-quaternions pngload)
  :pathname "src"
  :class :package-inferred-system)
