;;; turbo-colormap.el --- Turbo colormap function  -*- lexical-binding: t; -*-

;;; License: Apache 2.0

;;; ported by Claude Code from
;;; https://gist.github.com/mikhailov-work/0d177465a8151eb6ede1768d51d476c7

;; Port of TurboColormap from GLSL (tubo_colormap.glsl).
;; vec2/vec3/vec4 types are represented as plain lists of floats.

(defun turbo-saturate (x)
  "Clamp X to [0.0, 1.0], matching GLSL's saturate()."
  (min 1.0 (max 0.0 x)))

(defun turbo-dot (a b)
  "Dot product of lists A and B (sum of pairwise products)."
  (apply #'+ (cl-mapcar #'* a b)))

(defun turbo-colormap (x)
  "Map X in [0.0, 1.0] to an (R G B) color using the Turbo colormap.
Each component of the returned list is nominally in [0.0, 1.0], though
the polynomial approximation may produce values slightly outside that
range near the endpoints."
  (let* ((kRedVec4   '( 0.13572138   4.61539260 -42.66032258  132.13108234))
         (kGreenVec4 '( 0.09140261   2.19418839   4.84296658  -14.18503333))
         (kBlueVec4  '( 0.10667330  12.64194608 -60.58204836  110.36276771))
         (kRedVec2   '(-152.94239396  59.28637943))
         (kGreenVec2 '(   4.27729857   2.82956604))
         (kBlueVec2  '( -89.90310912  27.34824973))
         ;; x = saturate(x)
         (x  (turbo-saturate x))
         ;; v4 = vec4(1.0, x, x*x, x*x*x)
         (x2 (* x x))
         (x3 (* x x x))
         (v4 (list 1.0 x x2 x3))
         ;; v2 = v4.zw * v4.z
         ;; v4.zw extracts the z and w components as a vec2: (x2, x3)
         ;; multiplied by v4.z (= x2) gives (x4, x5)
         (v2 (list (* x2 x2) (* x3 x2))))
    (list (turbo-saturate (+ (turbo-dot v4 kRedVec4)   (turbo-dot v2 kRedVec2)))
          (turbo-saturate (+ (turbo-dot v4 kGreenVec4) (turbo-dot v2 kGreenVec2)))
          (turbo-saturate (+ (turbo-dot v4 kBlueVec4)  (turbo-dot v2 kBlueVec2))))))

(provide 'turbo-colormap)
;;; turbo-colormap.el ends here
