;;; dragonruby-stargate-tracker.el --- Stargate OS/Filesystem Guardian -*- lexical-binding: t -*-

;; Author: Macgyber <esteban3261g@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/Macgyber/dragonruby-mode

;;; Commentary:
;; This module implements the External Mutation Law (XII) and 
;; Authoritative Buffers Policy (XV). It monitors buffer integrity
;; via SHA-256 and prevents silent state corruption from external tools.
;; Hardened for v1.0 Blindada: Exact-Content Hashing and Event Registration.

;;; Code:

(require 'dragonruby-utils)

(defvar dragonruby-stargate-tracker--hashes (make-hash-table :test 'equal)
  "Dictionary of authoritative buffer hashes (path -> sha256).")

(defvar dragonruby-stargate-tracker-mismatch-hook nil
  "Hook run when an external mutation is detected.")

(defun dragonruby-stargate-tracker-verify ()
  "Verify the integrity of all project buffers against known hashes.
Returns nil if all matches, or a list of mismatched file paths."
  (let (mismatches)
    (maphash
     (lambda (path expected-hash)
       (let ((buffer (find-buffer-visiting path)))
         (when buffer
            (with-current-buffer buffer
             (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
                    ;; Law XII: EXACT content is sovereign. Whitespace normalization removed.
                    (current-hash (secure-hash 'sha256 content)))
               (unless (string-equal current-hash expected-hash)
                 (push path mismatches)))))))
     dragonruby-stargate-tracker--hashes)
    mismatches))

(defun dragonruby-stargate-tracker-update-hash (path)
  "Update the authoritative hash for a specific PATH."
  (let ((buffer (find-buffer-visiting path)))
    (if buffer
        (with-current-buffer buffer
          (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
                 (hash (secure-hash 'sha256 content)))
            (puthash path hash dragonruby-stargate-tracker--hashes)))
      (remhash path dragonruby-stargate-tracker--hashes))))

(defun dragonruby-stargate-tracker-pause (mismatches)
  "Halt the simulation due to integrity failure in MISMATCHES.
This is the 'Stasis Mode' required by Law XV. Registers failure in history."
  (message "🛑 STARGATE: Simulation HALTED. External Mutation Detected!")
  ;; Signal the Runtime of the exact files that were corrupted
  (let ((files-str (mapconcat #'file-name-nondirectory mismatches ", ")))
    (dragonruby-stargate-bridge-send-code 
     (format "Stargate::Clock.pause!; Stargate::Protocol.emit_event(type: 'external_mutation', files: %S)" 
             files-str)))
  (run-hooks 'dragonruby-stargate-tracker-mismatch-hook))

(defun dragonruby-stargate-tracker-monitor-all ()
  "Scan all project buffers and pause if a mismatch is found."
  (let ((mismatches (dragonruby-stargate-tracker-verify)))
    (when mismatches
      (dragonruby-stargate-tracker-pause mismatches)
      (dolist (path mismatches)
        (message "⚠️ Integrity Failure: %s was modified externally." (file-name-nondirectory path))))))

(provide 'dragonruby-stargate-tracker)
;;; dragonruby-stargate-tracker.el ends here
