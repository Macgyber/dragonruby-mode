;;; dragonruby-kernel.el --- The Central Nervous System of DragonRuby Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 DragonRuby Mode
;; Author: DragonRuby Mode Team
;; Keywords: tools, convenience, games

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; 🧱 THE USER CONTRACT
;;
;; 1. Paradigm Shift: Users MUST NOT (require ...) directly. 
;;    Interaction via (dragonruby-enable ...) ONLY.
;;
;; 2. Strict Lifecycle: Every module MUST have a clean disable function.
;;    Zero zombie state allowed.
;;
;; 3. The Three Laws:
;;    - Namespace Law: Modules MUST exclusively own their namespace.
;;    - Capability Law: Dependencies resolved by capabilities.
;;    - Cold Boot Law: Zero modules active at start.

;;; Code:

(require 'cl-lib)

;; -----------------------------------------------------------------------------
;; 🧠 Brain State (Registry)
;; -----------------------------------------------------------------------------

(defvar dragonruby--modules (make-hash-table :test 'equal)
  "Registry of all known modules. Key: Module Name (symbol). Value: Manifest (plist).")

(defvar-local dragonruby--active-modules nil
  "Registry of currently ACTIVE modules in this buffer. Key: Module Name (symbol). Value: t.")

(defun dragonruby--active-hash ()
  "Get the active modules hash for the current buffer safely.
Ensures we are always working with a valid hash table, avoiding stale references."
  (or (and (hash-table-p dragonruby--active-modules)
           dragonruby--active-modules)
      (setq dragonruby--active-modules (make-hash-table :test 'equal))))

(defvar dragonruby--capabilities (make-hash-table :test 'equal)
  "Map of capabilities to providing modules. Key: Capability (symbol). Value: List of Module Names.")

;; --- ⚡ SOVEREIGN KERNEL REGISTRY (The Ledger of Life) ---

(defvar dragonruby--live-timers nil "List of active timers registered with the Kernel.")
(defvar dragonruby--live-hooks nil "List of (hook . fn) pairs registered with the Kernel.")
(defvar dragonruby--live-processes nil "List of active processes registered with the Kernel.")

(defvar dragonruby--module-resources (make-hash-table :test 'equal)
  "Legacy registry for per-module resources. (Deprecated in favor of global ledger)")

(defun dragonruby-kernel-register-timer (timer)
  "Register a TIMER with the Kernel for lifecycle management.
Cleans defunct timers from the ledger during registration."
  (setq dragonruby--live-timers 
        (cl-remove-if-not #'timerp dragonruby--live-timers))
  (when (timerp timer)
    (push timer dragonruby--live-timers))
  timer)

(defun dragonruby-kernel-register-hook (hook fn &optional local)
  "Register a global or local HOOK function FN.
If LOCAL is non-nil, it is treated as a buffer-local hook."
  (add-hook hook fn nil local)
  ;; Clean orphaned hook references
  (setq dragonruby--live-hooks 
        (cl-remove-if (lambda (pair) 
                         (not (memq (cdr pair) (symbol-value (car pair)))))
                       dragonruby--live-hooks))
  (push (cons hook fn) dragonruby--live-hooks)
  fn)

(defun dragonruby-kernel-register-process (proc)
  "Register a PROCESS with the Kernel."
  (when (processp proc)
    (push proc dragonruby--live-processes))
  proc)

(defun dragonruby-register-resource (module type object)
  "Register a resource for a specific MODULE (Legacy)."
  (let ((existing (gethash module dragonruby--module-resources)))
    (puthash module (cons (cons type object) existing) dragonruby--module-resources)))

(defun dragonruby--kill-module-resources (module)
  "Kill all registered resources for MODULE."
  (let ((resources (gethash module dragonruby--module-resources)))
    (dolist (res resources)
      (let ((type (car res))
            (obj (cdr res)))
        (condition-case nil
            (pcase type
              ('timer (when (timerp obj) (cancel-timer obj)))
              ('process (when (processp obj) (delete-process obj)))
              ('hook (let ((hook-sym (car obj))
                           (fn (cdr obj)))
                       (remove-hook hook-sym fn t))))
          (error nil))))
    (remhash module dragonruby--module-resources)))

(defun dragonruby--unregister-module (name)
  "Remove all traces of NAME from the global index (Capabilities).
Prevents zombie providers after a hot-reload or module re-registration."
  (maphash
   (lambda (cap providers)
     (when (memq name providers)
       (puthash cap (delq name providers) dragonruby--capabilities)))
   dragonruby--capabilities))

(defun dragonruby--kernel-wipe-registry ()
  "INTERNAL: Wipe the registry and all state.
Only for Cold Boot or Unit Tests. NEVER for Hot Reload."
  (clrhash dragonruby--modules)
  (clrhash (dragonruby--active-hash))
  (clrhash dragonruby--capabilities)
  (message "🧠 Kernel: Registry Wiped (Cold Boot)."))

(defun dragonruby-kernel-buffer-cleanup ()
  "Surgically deactivate all modules in the current buffer only.
Does NOT affect other buffers or global Kernel state."
  (let* ((active-hash (dragonruby--active-hash))
         (active (cl-loop for k being the hash-keys in active-hash collect k)))
    (when active
      (message "🧹 Kernel: Cleaning up buffer [%s]..." (buffer-name))
      (dolist (name active)
        (ignore-errors (dragonruby-disable name)))
      ;; Also purge local hooks specifically registered here
      (dolist (pair dragonruby--live-hooks)
        (let ((hook (car pair)) (fn (cdr pair)))
          (remove-hook hook fn t))))))

(defun dragonruby-kernel-system-halt ()
  "OS-LEVEL EXORCISM: Kill all running DragonRuby activity globally.
Caution: This wipes state across all buffers."
  (interactive)
  (message "🧹 Kernel: GLOBAL SYSTEM HALT START...")

  ;; 1. Kill Timers
  (dolist (timer dragonruby--live-timers)
    (ignore-errors (cancel-timer timer)))
  (setq dragonruby--live-timers nil)

  ;; 2. Remove Global Hooks
  (dolist (pair dragonruby--live-hooks)
    (ignore-errors (remove-hook (car pair) (cdr pair))))
  (setq dragonruby--live-hooks nil)

  ;; 3. Kill Processes
  (dolist (proc dragonruby--live-processes)
    (ignore-errors (delete-process proc)))
  (setq dragonruby--live-processes nil)

  ;; 4. Module Cleanup across all buffers
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (bound-and-true-p dragonruby-mode)
        (dragonruby-kernel-buffer-cleanup))))

  ;; 5. Safety Net (Force kill any leaked timers)
  (dolist (timer (append timer-list timer-idle-list))
    (let* ((fn (timer--function timer))
           (fn-name (cond ((symbolp fn) (symbol-name fn))
                          (t ""))))
      (when (string-prefix-p "dragonruby-" fn-name)
        (cancel-timer timer))))

  (message "🧹 Kernel: Global System Halted."))

(defun dragonruby-kernel-reset-live ()
  "Deprecated: Use dragonruby-kernel-system-halt instead."
  (interactive)
  (dragonruby-kernel-system-halt))

(defun dragonruby-kernel-system-shutdown ()
  "Full system shutdown."
  (interactive)
  (dragonruby-kernel-reset-live)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (bound-and-true-p dragonruby-mode)
        (dragonruby-mode -1))))
  (clrhash dragonruby--module-resources)
  (message "🐲 Kernel: Shutdown Complete."))

;; -----------------------------------------------------------------------------
;; 📜 Manifest Registration
;; -----------------------------------------------------------------------------

(cl-defun dragonruby-register-module (&key name type namespace provides requires entry-point enable-fn disable-fn)
  "Register a module with the Sovereign Kernel (v0.7.3 Standard).

The module MUST adhere to the Life-Cycle Contract:
1. Every Hook, Timer, or Process created in :ENABLE-FN must be registered 
   using `dragonruby-kernel-register-*` family functions.
2. :DISABLE-FN should ideally handle local cleanup, but the Kernel reset 
   is the ultimate authority.

:NAME        - Symbol. Unique ID (e.g. 'sprites).
:TYPE        - :main or :tool.
:NAMESPACE   - String. Strict prefix (e.g. \"dragonruby-sprite-\").
:PROVIDES    - List of symbols. Capabilities (e.g. '(:visuals)).
:REQUIRES    - List of symbols. Dependencies.
:ENTRY-POINT - Symbol. The feature to `require`.
:ENABLE-FN   - Function. Setup logic.
:DISABLE-FN  - Function. Cleanup logic."
  
  ;; Validation
  (unless (and name entry-point enable-fn disable-fn)
    (error "🧠 Kernel: Module %s violates Contract. Missing core fields." name))
  
  ;; Namespace Law (STRICT REJECTION)
  (when namespace
    (unless (string-prefix-p "dragonruby-" namespace)
      (error "🧠 Kernel: REJECTED Module %s. Namespace '%s' violates Law (MUST start with dragonruby-)." name namespace)))
  ;; Register (Garbage Collection: unregister old providers first)
  (when (gethash name dragonruby--modules)
    (dragonruby--unregister-module name))

  (puthash name (list :name name
                      :type type
                      :namespace namespace
                      :provides provides
                      :requires requires
                      :entry-point entry-point
                      :enable-fn enable-fn
                      :disable-fn disable-fn)
           dragonruby--modules)

  ;; Index Capabilities
  (dolist (cap provides)
    (let ((existing (gethash cap dragonruby--capabilities)))
      (unless (memq name existing)
        (puthash cap (cons name existing) dragonruby--capabilities))))

  (message "🧠 Kernel: Registered Module [%s]" name))

;; -----------------------------------------------------------------------------
;; 🔌 Dependency Resolution (Capabilities)
;; -----------------------------------------------------------------------------

(defun dragonruby--find-provider (capability)
  "Finding a provider for CAPABILITY. Prefers already active modules."
  (let ((providers (gethash capability dragonruby--capabilities)))
    (unless providers
      (error "🧠 Kernel: No module provides capability: %s" capability))
    ;; Prefer active
    (or (cl-find-if (lambda (m) (gethash m (dragonruby--active-hash))) providers)
        (car providers))))

(defun dragonruby--resolve-dependencies (module-name stack)
  "Recursively ensure dependencies for MODULE-NAME are met.
STACK prevents circular dependencies."
  (let* ((manifest (gethash module-name dragonruby--modules))
         (reqs (plist-get manifest :requires)))
    (unless manifest (error "🧠 Kernel: Unknown module %s" module-name))

    (dolist (cap reqs)
      (dragonruby-enable-capability cap (cons module-name stack)))))

;; -----------------------------------------------------------------------------
;; 🧬 Lifecycle (Activation)
;; -----------------------------------------------------------------------------

(defun dragonruby-enable-capability (capability &optional stack)
  "Ensure CAPABILITY is available."
  (let ((provider (dragonruby--find-provider capability)))
    (if (gethash provider (dragonruby--active-hash))
        provider
      (dragonruby-enable provider stack)
      provider)))

(defun dragonruby-enable (module-name &optional stack)
  "Activate MODULE-NAME and its dependencies for the current buffer."
  (message "🧠 Kernel: Processing ENABLE [%s]..." module-name)
  (let ((active-hash (dragonruby--active-hash))
        (stack (or stack (list module-name))))
    (unless (gethash module-name active-hash)
      ;; Cycle Check
      (when (member module-name (cdr stack))
        (error "🧠 Kernel: Circular dependency detected: %s -> %s" module-name stack))

      (let ((manifest (gethash module-name dragonruby--modules)))
        (unless manifest (error "🧠 Kernel: Unknown module %s" module-name))

        ;; CRASH-SAFETY ROLLBACK
        (condition-case err
            (progn
              ;; 1. Resolve Dependencies
              (dragonruby--resolve-dependencies module-name stack)

              ;; 2. Load Code (Entry Point)
              (require (plist-get manifest :entry-point))

              ;; 3. Execute Contract
              (let ((enable-fn (plist-get manifest :enable-fn)))
                (if (functionp enable-fn)
                    (funcall enable-fn)
                  (error "🧠 Kernel: %s :enable-fn is not callable" module-name)))

              ;; 4. Mark Active
              (puthash module-name t active-hash)
              (message "🧠 Kernel: Module [%s] ENABLED" module-name))
          (error
           (dragonruby-disable module-name)
           (signal (car err) (cdr err))))))))

;; -----------------------------------------------------------------------------
;; 🧯 Lifecycle (Deactivation)
;; -----------------------------------------------------------------------------

(defun dragonruby-disable (module-name)
  "Deactivate MODULE-NAME for the current buffer."
  (let ((active-hash (dragonruby--active-hash)))
    (when (gethash module-name active-hash)
      (let ((my-caps (plist-get (gethash module-name dragonruby--modules) :provides)))
      
      ;; 1. Cascade Check
      (maphash 
       (lambda (dep-name _active)
         (unless (eq dep-name module-name)
           (let* ((dep-manifest (gethash dep-name dragonruby--modules))
                  (dep-reqs (plist-get dep-manifest :requires)))
             
             ;; Does dependent need me?
             (when (cl-intersection dep-reqs my-caps)
               ;; Can anyone else satisfy the need?
               (let ((satisfied t))
                 (dolist (req dep-reqs)
                   (when (memq req my-caps)
                     ;; Check other active providers
                     (let ((providers (gethash req dragonruby--capabilities)))
                       (unless (cl-some (lambda (p) 
                                          (and (not (eq p module-name))
                                               (gethash p active-hash)))
                                        providers)
                         (setq satisfied nil)))))
                 
                 (unless satisfied
                   (message "🧠 Kernel: Cascade disabling [%s] (needs %s provided by %s)" 
                            dep-name my-caps module-name)
                   (dragonruby-disable dep-name)))))))
       active-hash))

    ;; 2. Execute Contract & Kill Resources
    (let* ((manifest (gethash module-name dragonruby--modules))
           (disable-fn (and manifest (plist-get manifest :disable-fn))))
      (when (functionp disable-fn)
        (funcall disable-fn))
      ;; HYGIENE: Kill registered timers/processes
      (dragonruby--kill-module-resources module-name))

    ;; 3. Mark Inactive
    (remhash module-name active-hash)
    (message "🧠 Kernel: Module [%s] DISABLED and PURGED" module-name))))

(defun dragonruby-kernel-get-registered-modules ()
  "Return a list of all registered module manifests."
  (let (acc)
    (maphash (lambda (_k v) (push v acc)) dragonruby--modules)
    (reverse acc)))

(defun dragonruby-module-status (module-name)
  (if (gethash module-name (dragonruby--active-hash)) :active :inactive))

(provide 'dragonruby-kernel)
;;; dragonruby-kernel.el ends here
