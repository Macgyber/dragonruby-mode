;;; dragonruby-stargate-parser.el --- Stargate Event & JSON Parser -*- lexical-binding: t -*-

(require 'json)

(defun dragonruby-stargate-parser-event (payload)
  "Parse a raw PAYLOAD from DragonRuby and identify Stargate events."
  (condition-case err
      (let ((json-start (string-match "{\\(?:.*\\)\"type\"" payload)))
        (if json-start
            (let* ((json-string (substring payload (string-match "{" payload)))
                   (json-object-type 'alist)
                   (json-key-type 'string)
                   (event (json-read-from-string json-string)))
              (dragonruby-stargate-parser--dispatch event))
          
          ;; Fallback for non-JSON signals (ACKs)
          (dragonruby-stargate-parser--dispatch-raw payload)))
    (error
     (when (and (boundp 'dragonruby-stargate-bridge--debug) dragonruby-stargate-bridge--debug)
       (message "📡 [PARSER] Error: %S in payload: %S" err payload)))))

(defun dragonruby-stargate-parser--dispatch (event)
  "Route a parsed JSON EVENT to the manager."
  (let ((type (cdr (assoc "type" event))))
    (cond
     ((string= type "divergence")
      (message "⚡ STARGATE: DIVERGENCE DETECTED!")
      (run-hook-with-args 'dragonruby-stargate-bridge-event-hook event))
     ((string= type "pong")
      (message "📡 STARGATE: Heartbeat confirmed."))
     (t
      (run-hook-with-args 'dragonruby-stargate-bridge-event-hook event)))))

(defun dragonruby-stargate-parser--dispatch-raw (payload)
  "Identify raw string signals in PAYLOAD."
  (cond
   ((string-match "STARGATE: \\[BOOT\\] Initializing" payload)
    (message "📡 STARGATE: Sequence detected."))
   ((string-match "STARGATE: Simulation PAUSED" payload)
    (run-hook-with-args 'dragonruby-stargate-bridge-event-hook '((type . "stasis_ack"))))
   ((string-match "STARGATE: Simulation RESUMED" payload)
    (run-hook-with-args 'dragonruby-stargate-bridge-event-hook '((type . "record_ack"))))))

(provide 'dragonruby-stargate-parser)
