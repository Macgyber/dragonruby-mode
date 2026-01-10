;;; dragonruby-font-engine.el --- Logic for rendering font previews -*- lexical-binding: t; -*-

(require 'dragonruby-core)

(defun dragonruby--render-font-sheet (font-path output-path &optional template)
  "Render a font sample sheet for FONT-PATH to OUTPUT-PATH.
TEMPLATE can be 'pangram, 'abc, 'full, or a custom string."
  (let* ((cmd (if (executable-find "magick") "magick" "convert"))
         (text (cond
                ((eq template 'pangram) "The quick brown fox jumps over the lazy dog.\nTHE QUICK BROWN FOX JUMPS OVER THE LAZY DOG.")
                ((eq template 'abc) "ABCDEFGHIJKLMNOPQRSTUVWXYZ\nabcdefghijklmnopqrstuvwxyz\n0123456789\n!@#$%^&*()_+-=[]{}|;':\",./<>?")
                ((stringp template) template)  ;; Custom user text
                (t "DragonRuby Game Toolkit\n\nAa Bb Cc Dd Ee Ff Gg Hh\nIi Jj Kk Ll Mm Nn Oo Pp\nQq Rr Ss Tt Uu Vv Ww Xx\nYy Zz 0123456789")))
         (pointsize (cond ((eq template 'pangram) "24")
                          ((stringp template) "28")  ;; Larger for custom text
                          (t "18"))))
    
    (call-process cmd nil nil nil
                  "-background" "#282C34"
                  "-fill" "#FFFFFF"
                  "-font" (expand-file-name font-path)
                  "-pointsize" pointsize
                  "-size" "800x"
                  "-gravity" "center"
                  (format "label:%s" text)
                  "-bordercolor" "#282C34"
                  "-border" "20x20"
                  (expand-file-name output-path))))

(provide 'dragonruby-font-engine)
;;; dragonruby-font-engine.el ends here
