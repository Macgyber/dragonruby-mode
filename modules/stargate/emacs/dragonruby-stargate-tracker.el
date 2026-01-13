;;; tracker.el --- Stargate OS/Filesystem Guardian -*- lexical-binding: t -*-

;;; Commentary:
;; This module implements the External Mutation Law (XII) and 
;; Authoritative Buffers Policy (XV). It monitors buffer integrity
;; via SHA-256 and prevents silent state corruption from external tools.

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
             (let ((current-hash (secure-hash 'sha256 (buffer-substring-no-properties (point-min) (point-max)))))
               (unless (string-equal current-hash expected-hash)
                 (push path mismatches)))))))
     dragonruby-stargate-tracker--hashes)
    mismatches))

(defun dragonruby-stargate-tracker-update-hash (path)
  "Update the authoritative hash for a specific PATH."
  (let ((buffer (find-buffer-visiting path)))
    (if buffer
        (with-current-buffer buffer
          (let ((hash (secure-hash 'sha256 (buffer-substring-no-properties (point-min) (point-max)))))
            (puthash path hash dragonruby-stargate-tracker--hashes)))
      (remhash path dragonruby-stargate-tracker--hashes))))

(defun dragonruby-stargate-tracker-pause ()
  "Halt the simulation due to integrity failure.
This is the 'Stasis Mode' required by Law XV."
  (message "🛑 STARGATE: Simulation HALTED. External Mutation Detected!")
  ;; TODO: Send explicit PAUSE command to the Ruby Runtime via Protocol bridge
  (run-hooks 'dragonruby-stargate-tracker-mismatch-hook))

(defun dragonruby-stargate-tracker-monitor-all ()
  "Scan all project buffers and pause if a mismatch is found."
  (let ((mismatches (dragonruby-stargate-tracker-verify)))
    (when mismatches
      (dragonruby-stargate-tracker-pause)
      (dolist (path mismatches)
        (message "⚠️ Integrity Failure: %s was modified externally." (file-name-nondirectory path))))))

(provide 'dragonruby-stargate-tracker)
;;; tracker.el ends here
