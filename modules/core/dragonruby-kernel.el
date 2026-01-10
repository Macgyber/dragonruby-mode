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

(defvar dragonruby--active-modules (make-hash-table :test 'equal)
  "Registry of currently ACTIVE modules. Key: Module Name (symbol). Value: t.")

(defvar dragonruby--capabilities (make-hash-table :test 'equal)
  "Map of capabilities to providing modules. Key: Capability (symbol). Value: List of Module Names.")

(defun dragonruby--reset-kernel ()
  "HARD RESET of the Kernel. Tabula Rasa."
  (clrhash dragonruby--modules)
  (clrhash dragonruby--active-modules)
  (clrhash dragonruby--capabilities)
  (message "🧠 Kernel: System Reset Complete."))

;; -----------------------------------------------------------------------------
;; 📜 Manifest Registration
;; -----------------------------------------------------------------------------

(cl-defun dragonruby-register-module (&key name type namespace provides requires entry-point enable-fn disable-fn)
  "Register a module with the Kernel.

:NAME        - Symbol. Unique ID.
:TYPE        - :main or :tool.
:NAMESPACE   - String. Strict prefix (e.g. \"dragonruby-sprites-\").
               Modules MUST exclusively own this namespace.
:PROVIDES    - List of symbols. Capabilities offered.
:REQUIRES    - List of symbols. Capabilities needed.
:ENTRY-POINT - Symbol. The Emacs feature to (require ...).
:ENABLE-FN   - Function. Sets up hooks, timers, state.
:DISABLE-FN  - Function. Cleans up hooks, timers, state."
  
  ;; Validation
  (unless (and name entry-point enable-fn disable-fn)
    (error "🧠 Kernel: Module %s violates Contract. Missing core fields." name))
  
  ;; Namespace Law (STRICT REJECTION)
  (when namespace
    (unless (string-prefix-p "dragonruby-" namespace)
      (error "🧠 Kernel: REJECTED Module %s. Namespace '%s' violates Law (MUST start with dragonruby-)." name namespace)))

  ;; Register
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
      (puthash cap (cons name existing) dragonruby--capabilities)))

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
    (or (cl-find-if (lambda (m) (gethash m dragonruby--active-modules)) providers)
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
    (if (gethash provider dragonruby--active-modules)
        provider
      (dragonruby-enable provider stack)
      provider)))

(defun dragonruby-enable (module-name &optional stack)
  "Activate MODULE-NAME and its dependencies."
  (unless (gethash module-name dragonruby--active-modules)
    ;; Cycle Check
    (when (member module-name stack)
      (error "🧠 Kernel: Circular dependency detected: %s -> %s" module-name stack))

    (let ((manifest (gethash module-name dragonruby--modules)))
      (unless manifest (error "🧠 Kernel: Unknown module %s" module-name))

      ;; 1. Resolve Dependencies (Pass stack as-is, let resolve add itself for children)
      (dragonruby--resolve-dependencies module-name stack)

      ;; 2. Load Code (Entry Point)
      (require (plist-get manifest :entry-point))

      ;; 3. Execute Contract
      (let ((enable-fn (plist-get manifest :enable-fn)))
        (if (functionp enable-fn)
            (funcall enable-fn)
          (error "🧠 Kernel: %s :enable-fn is not callable" module-name)))

      ;; 4. Mark Active
      (puthash module-name t dragonruby--active-modules)
      (message "🧠 Kernel: Module [%s] ENABLED" module-name))))

;; -----------------------------------------------------------------------------
;; 🧯 Lifecycle (Deactivation)
;; -----------------------------------------------------------------------------

(defun dragonruby-disable (module-name)
  "Deactivate MODULE-NAME with Smart Cascade."
  (when (gethash module-name dragonruby--active-modules)
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
                                               (gethash p dragonruby--active-modules)))
                                        providers)
                         (setq satisfied nil)))))
                 
                 (unless satisfied
                   (message "🧠 Kernel: Cascade disabling [%s] (needs %s provided by %s)" 
                            dep-name my-caps module-name)
                   (dragonruby-disable dep-name)))))))
       dragonruby--active-modules))

    ;; 2. Execute Contract
    (let* ((manifest (gethash module-name dragonruby--modules))
           (disable-fn (plist-get manifest :disable-fn)))
      (if (functionp disable-fn)
          (funcall disable-fn)
        (error "🧠 Kernel: %s :disable-fn is not callable" module-name)))

    ;; 3. Mark Inactive
    (remhash module-name dragonruby--active-modules)
    (message "🧠 Kernel: Module [%s] DISABLED" module-name)))

(defun dragonruby-module-status (module-name)
  (if (gethash module-name dragonruby--active-modules) :active :inactive))

(provide 'dragonruby-kernel)
;;; dragonruby-kernel.el ends here
