;;; dragonruby-color-utils.el --- Color math and conversion utilities -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun dragonruby--valid-rgba-p (r g b a)
  "Check if R G B and optional A are within 0-255 range."
  (and (numberp r) (<= 0 r 255)
       (numberp g) (<= 0 g 255)
       (numberp b) (<= 0 b 255)
       (or (null a) (and (numberp a) (<= 0 a 255)))))

(defun dragonruby--apply-alpha (r g b a)
  "Apply alpha A to R G B values."
  (if a
      (let ((alpha (/ a 255.0)))
        (list (round (* r alpha))
              (round (* g alpha))
              (round (* b alpha))))
    (list r g b)))

(defun dragonruby--rgb-to-hex (r g b)
  "Convert RGB values (0-255) to a Hex string #RRGGBB."
  (format "#%02x%02x%02x" (min 255 (max 0 r)) (min 255 (max 0 g)) (min 255 (max 0 b))))

(defun dragonruby--get-contrast-color (r g b)
  "Return \\='white\\=' or \\='black\\=' depending on the brightness of the background color."
  (if (< (+ (* r 0.299) (* g 0.587) (* b 0.114)) 128)
      "white"
    "black"))

(defun dragonruby--in-code-p ()
  "Check if point is in code (not in string or comment)."
  (let ((state (syntax-ppss)))
    (and (not (nth 3 state))
         (not (nth 4 state)))))

(provide 'dragonruby-color-utils)
