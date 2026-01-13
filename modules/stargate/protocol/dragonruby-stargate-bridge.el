;;; bridge.el --- Stargate Communication Bridge -*- lexical-binding: t -*-

;;; Commentary:
;; This module implements the "The Word" (Protocol Layer).
;; It handles the transmission of code snippets from Emacs to DragonRuby
;; and the reception of simulation events (Moments) from DragonRuby to Emacs.

(require 'json)
(require 'dragonruby-utils)

(defvar dragonruby-stargate-bridge--process nil
  "The active bridge process.")

(defvar dragonruby-stargate-bridge-event-hook nil
  "Hook run when a Stargate event is received from the runtime.")

(defvar-local dragonruby-stargate-bridge--partial-buffer ""
  "Buffer for reassembling fragmented JSON messages.")

(defun dragonruby-stargate-bridge-send-code (code)
  "Send a snippet of CODE to the DragonRuby runtime for injection."
  (let ((wrapped-code (format "Stargate::Injection.schedule %S" code)))
    (dragonruby-stargate-bridge--transmit wrapped-code)))

(defun dragonruby-stargate-bridge--transmit (string)
  "Physically transmit STRING to the DragonRuby process."
  (let ((repl-file (expand-file-name "repl.rb" (dragonruby--find-project-root))))
    (with-temp-file repl-file
      (insert string))
    (message "📡 Stargate Bridge: Transmitted to %s" (file-name-nondirectory repl-file))))

(defun dragonruby-stargate-bridge-install (process)
  "Install the Stargate filter on the DragonRuby PROCESS."
  (setq dragonruby-stargate-bridge--process process)
  (let ((old-filter (process-filter process)))
    (set-process-filter process
                        (lambda (proc string)
                          (when old-filter (funcall old-filter proc string))
                          (dragonruby-stargate-bridge--filter proc string))))
  (message "📡 Stargate Bridge: Chronicler cabled to %s" (process-name process)))

(defun dragonruby-stargate-bridge-find-and-install (&optional silent)
  "Search for the DragonRuby process and install the bridge if it exists."
  (interactive)
  (let ((proc (cl-find-if (lambda (p) 
                           (let ((name (process-name p))
                                 (cmd (mapconcat #'identity (process-command p) " ")))
                             (or (string-match-p "[Dd]ragon" name)
                                 (string-match-p "[Rr]uby" name)
                                 (string-match-p "[Dd]ragon" cmd)
                                 (string-match-p "[Rr]uby" cmd)
                                 (string-match-p "dr-game" name))))
                          (process-list))))
    (if (and proc (process-live-p proc))
        (dragonruby-stargate-bridge-install proc)
      (unless silent
        (message "⚠️ Stargate Bridge: Searching for DragonRuby process... (Not found)")))))

(defun dragonruby-stargate-bridge--filter (process output)
  "Filter and capture [STARGATE_MOMENT] events from PROCESS OUTPUT."
  (setq dragonruby-stargate-bridge--partial-buffer 
        (concat dragonruby-stargate-bridge--partial-buffer output))
  
  ;; Process line by line
  (while (string-match "\\[STARGATE_MOMENT\\] \\({[^\r\n]*}\\)[\r\n]+" dragonruby-stargate-bridge--partial-buffer)
    (let ((payload (match-string 1 dragonruby-stargate-bridge--partial-buffer))
          (end-pos (match-end 0)))
      ;; Remove the processed part
      (setq dragonruby-stargate-bridge--partial-buffer 
            (substring dragonruby-stargate-bridge--partial-buffer end-pos))
      (dragonruby-stargate-bridge-handle-event payload)))
  
  ;; Safety cleanup
  (when (> (length dragonruby-stargate-bridge--partial-buffer) 100000)
    (setq dragonruby-stargate-bridge--partial-buffer "")))

(defun dragonruby-stargate-bridge-handle-event (json-string)
  "Parse and distribute a JSON-STRING event from the runtime."
  (condition-case err
      (let* ((json-object-type 'alist)
             (event (json-read-from-string json-string)))
        (run-hook-with-args 'dragonruby-stargate-bridge-event-hook event))
    (error
     (message "❌ Stargate Bridge: Failed to parse event: %s" err))))

(provide 'dragonruby-stargate-bridge)
;;; bridge.el ends here
