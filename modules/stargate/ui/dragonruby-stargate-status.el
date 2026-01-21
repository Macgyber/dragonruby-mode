;;; dragonruby-stargate-status.el --- Stargate Status Dashboard Controller -*- lexical-binding: t -*-

(require 'dragonruby-stargate-status-view)
(require 'dragonruby-stargate-telemetry)
(require 'dragonruby-stargate-recorder)
(require 'dragonruby-stargate-timeline)

(defvar dragonruby-stargate-status-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'dragonruby-stargate-status-refresh)
    (define-key map (kbd "i") #'dragonruby-stargate--initialize-runtime)
    (define-key map (kbd "r") #'dragonruby-stargate-record)
    (define-key map (kbd "<f7>") #'dragonruby-stargate-record)
    (define-key map (kbd "p") #'dragonruby-stargate-pause)
    (define-key map (kbd "<f8>") #'dragonruby-stargate-pause)
    (define-key map (kbd "t") #'dragonruby-stargate-timeline)
    (define-key map (kbd "<f9>") #'dragonruby-stargate-timeline)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode dragonruby-stargate-status-mode special-mode "Stargate-Analyzer")

(defun dragonruby-stargate-status-buffer ()
  "Show the classic Stargate status dashboard."
  (interactive)
  (let ((buf (get-buffer-create "*Stargate Status*")))
    (with-current-buffer buf
      (dragonruby-stargate-status-mode)
      (dragonruby-stargate-status-refresh))
    (display-buffer buf)))

(defun dragonruby-stargate-status-refresh ()
  "Refresh the dashboard via the Status View expert."
  (interactive)
  (when (get-buffer "*Stargate Status*")
    (with-current-buffer "*Stargate Status*"
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dragonruby-stargate-status-view--render)
        (goto-char (point-min))))))

(add-hook 'dragonruby-stargate-session-updated-hook #'dragonruby-stargate-status-refresh)

(provide 'dragonruby-stargate-status)
