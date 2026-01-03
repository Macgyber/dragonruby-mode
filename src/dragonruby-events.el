;;; dragonruby-events.el --- Lightweight internal event bus -*- lexical-binding: t; -*-

;; A semantic layer over Emacs hooks to decouple modules.
;; Modules publish events, other modules subscribe.
;;
;; Usage:
;; (dragonruby-events-on 'sprite:click #'my-handler)
;; (dragonruby-events-emit 'sprite:click sprite-path)

(defvar dragonruby-events-listeners (make-hash-table :test 'eq)
  "Registry of event listeners.
Keys are event symbols, values are lists of functions.")

(defun dragonruby-events-on (event-name callback)
  "Subscribe CALLBACK to EVENT-NAME."
  (let ((current (gethash event-name dragonruby-events-listeners)))
    (puthash event-name (append current (list callback)) dragonruby-events-listeners)))

(defun dragonruby-events-off (event-name callback)
  "Unsubscribe CALLBACK from EVENT-NAME."
  (let ((current (gethash event-name dragonruby-events-listeners)))
    (puthash event-name (remove callback current) dragonruby-events-listeners)))

(defun dragonruby-events-emit (event-name &rest args)
  "Trigger EVENT-NAME with ARGS. Notifies all subscribers synchronously."
  (let ((listeners (gethash event-name dragonruby-events-listeners)))
    (dolist (fn listeners)
      (condition-case err
          (apply fn args)
        (error (message "DragonRuby Event Error [%s]: %S" event-name err))))))

(defun dragonruby-events-clear (event-name)
  "Clear all listeners for EVENT-NAME."
  (remhash event-name dragonruby-events-listeners))

(provide 'dragonruby-events)
