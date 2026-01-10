;;; dragonruby-audio-fs.el --- Filesystem operations for audio -*- lexical-binding: t; -*-

(require 'dragonruby-core)

(defun dragonruby--get-all-sounds-in-project ()
  "Get all sound files in the project."
  (dragonruby--collect-project-files 'audio))

(provide 'dragonruby-audio-fs)
;;; dragonruby-audio-fs.el ends here
