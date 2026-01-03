;;; dragonruby-sprite-actions.el --- Interaction logic for sprites -*- lexical-binding: t; -*-

(require 'dragonruby-core)

(declare-function w32-shell-execute "w32fns.c")

(defun dragonruby-jump-to-sprite-source (path)
  "Open PATH inside Emacs image viewer.
This ensures the user stays within the Emacs environment for navigation."
  (interactive "fOpen sprite: ")
  (if (file-exists-p path)
      (progn
        (find-file path)
        (message "Navigation: Opened [%s] in Emacs viewer" (file-name-nondirectory path)))
    (message "Error: File not found [%s]" path)))

(provide 'dragonruby-sprite-actions)
;;; dragonruby-sprite-actions.el ends here
