;;; dragonruby-stargate-bridge.el --- Stargate Process Bridge -*- lexical-binding: t -*-

(require 'dragonruby-stargate-parser)

(defvar dragonruby-stargate-bridge--process nil)
(defvar dragonruby-stargate-bridge--debug nil)

(defun dragonruby-stargate-bridge--filter (process output)
  "Minimalist output aggregator. Forwards everything to the Parser."
  (let ((sim-buf (process-buffer process)))
    ;; 1. Visibility Mirror
    (when (buffer-live-p sim-buf)
      (with-current-buffer sim-buf
        (save-excursion
          (goto-char (point-max))
          (let ((inhibit-read-only t))
            (insert output)))))

    ;; 2. Line processing loop for the Parser
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (dragonruby-stargate-parser-event line)
          (forward-line 1))))))

(defun dragonruby-stargate-bridge-find-and-install (&optional manual-find silent)
  "Attach the bridge filter to the DragonRuby process."
  (let ((proc (or (get-process "dragonruby") 
                  (and manual-find (seq-find (lambda (p) (string-match-p "dragonruby" (process-name p))) (process-list))))))
    (if (and proc (process-live-p proc))
        (progn
          (setq dragonruby-stargate-bridge--process proc)
          (set-process-filter proc #'dragonruby-stargate-bridge--filter)
          (set-process-sentinel proc #'dragonruby-stargate-bridge--sentinel)
          (unless silent (message "📡 STARGATE: Bridge cabled to %s" (process-name proc)))
          t)
      (unless silent (message "❌ STARGATE: No process found."))
      nil)))

(defun dragonruby-stargate-bridge--sentinel (process event)
  "Cleanup when the DragonRuby process ends."
  (message "📡 STARGATE: Process %s | Event: %s" (process-name process) (string-trim event))
  (when (member (process-status process) '(exit signal closed))
    (setq dragonruby-stargate-bridge--process nil)
    (setq dragonruby-stargate--runtime-ready nil)
    (dragonruby-stargate--set-state nil)
    (message "🌌 STARGATE: Portals safely closed (Engine Detached).")))

(provide 'dragonruby-stargate-bridge)
