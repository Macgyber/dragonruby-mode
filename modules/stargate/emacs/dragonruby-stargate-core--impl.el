;;; dragonruby-stargate-core--impl.el --- Stargate Private Implementation -*- lexical-binding: t -*-

(require 'dragonruby-stargate-portal)
(require 'dragonruby-stargate-monitor)
(require 'dragonruby-stargate-bridge)
(require 'dragonruby-stargate-fsm)
(require 'dragonruby-stargate-sessions)
(require 'dragonruby-stargate-telemetry)
(require 'dragonruby-stargate-status)
(require 'dragonruby-stargate-recorder)

(defvar dragonruby-stargate--runtime-ready nil)
(defvar-local dragonruby-stargate--divergence-detected nil)

;;; --- Lifecycle (Kernel Hooks) ---

(defun dragonruby-stargate-enable ()
  "Activate the Stargate system."
  (dragonruby-stargate-bridge-find-and-install nil t)
  (dragonruby-stargate--initialize-runtime)
  (message "🌌 STARGATE: Systems Online."))

(defun dragonruby-stargate-disable ()
  "Shutdown the Stargate system."
  (dragonruby-stargate-session-stop)
  (dragonruby-stargate--set-state nil)
  (message "🌌 STARGATE: Systems Offline."))

;;; --- Orchestration ---

(defun dragonruby-stargate--initialize-runtime ()
  "Initialize the portal and establish authority."
  (interactive)
  (let ((session (dragonruby-stargate--ensure-session)))
    (if (not session)
        (message "❌ STARGATE: Cannot initialize, session lost.")
      (progn
        (dragonruby-stargate-portal-create)
        (dragonruby-stargate--set-state :initializing)))))

(defun dragonruby-stargate--ensure-session ()
  "Ensure project root and active session exist."
  (let ((root (or (dragonruby--find-project-root t) dragonruby--last-detected-project-root)))
    (when root
      (unless dragonruby-stargate--active-session
        (dragonruby-stargate-session-init root))
      dragonruby-stargate--active-session)))

(provide 'dragonruby-stargate-core--impl)
