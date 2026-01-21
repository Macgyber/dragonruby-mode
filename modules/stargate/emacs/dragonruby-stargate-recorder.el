;;; dragonruby-stargate-recorder.el --- Stargate Recording & Command Controller -*- lexical-binding: t -*-

(require 'dragonruby-utils)

(defun dragonruby-stargate-recorder--send (cmd)
  "Internal: Write CMD to the stargate_cmd.rb file (Law XIX)."
  (let* ((root (or (dragonruby--find-project-root t) dragonruby--last-detected-project-root))
         (cmd-file (expand-file-name "mygame/stargate_cmd.rb" root)))
    (if (not root)
        (message "❌ STARGATE: No project root found for command.")
      (with-temp-file cmd-file
        (insert (format "# STARGATE COMMAND: %s\n" (current-time-string)))
        (insert cmd)
        (insert "\n"))
      (message "🌌 STARGATE >> %s" cmd))))

(defun dragonruby-stargate--send-active-command (code)
  "Generic: Send CODE to the runtime command bus."
  (dragonruby-stargate-recorder--send code))

(defun dragonruby-stargate-inject-buffer ()
  "Inject current buffer content into the Stargate runtime (Law VI)."
  (interactive)
  (let ((code (buffer-substring-no-properties (point-min) (point-max))))
    (dragonruby-stargate--send-active-command 
     (format "Stargate::Injection.schedule(%S)" code))))

(defun dragonruby-stargate-record ()
  "Command: Tell DragonRuby to start/resume recording."
  (interactive)
  (dragonruby-stargate-recorder--send "Stargate::Clock.resume!"))

(defun dragonruby-stargate-pause ()
  "Command: Tell DragonRuby to pause simulation."
  (interactive)
  (dragonruby-stargate-recorder--send "Stargate::Clock.pause!"))

(defun dragonruby-stargate-emergency-halt ()
  "Command: Forcefully terminate DragonRuby process (System Level)."
  (interactive)
  (message "🛑 STARGATE: EMERGENCY HALT INITIATED...")
  (shell-command "taskkill /F /IM dragonruby.exe /T")
  (when (boundp 'dragonruby-stargate-bridge--process)
    (setq dragonruby-stargate-bridge--process nil))
  (message "🌌 STARGATE: DragonRuby process terminated."))

(provide 'dragonruby-stargate-recorder)
