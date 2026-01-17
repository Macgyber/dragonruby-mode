;;; timeline.el --- Stargate Branch Forest Visualizer -*- lexical-binding: t -*-

;;; Commentary:
;; This module implements the "Perception" of the Branch Forest (Law XVI).
;; It renders the timeline graph and handles scrubbing/jumping between Moments.

(require 'dragonruby-stargate-bridge)
(require 'dragonruby-stargate-manager)

(defvar-local dragonruby-stargate-timeline--buffer nil
  "The buffer currently being visualized.")

(defvar dragonruby-stargate-timeline-refresh-delay 0.5
  "Seconds to wait before refreshing the timeline after an event.")

(defvar dragonruby-stargate-timeline--refresh-timer nil
  "Timer for debouncing timeline refreshes.")

(defvar dragonruby-stargate-timeline--last-jump nil
  "The last address jumped to, for highlighting.")

;; --- FACES ---

(defface dragonruby-stargate-branch-face
  '((t :inherit font-lock-function-name-face :weight bold :height 1.1))
  "Face for branch headers in the timeline.")

(defface dragonruby-stargate-frame-face
  '((t :inherit font-lock-constant-face))
  "Face for frame entries in the timeline.")

(defface dragonruby-stargate-divergence-face
  '((t :inherit error :slant italic))
  "Face for divergence markers.")

(defface dragonruby-stargate-current-face
  '((t :inherit highlight :box (:line-width 1 :color "green")))
  "Face for the currently active/selected moment.")

(defun dragonruby-stargate-timeline ()
  "Render the Branch Forest visualization for the current session."
  (interactive)
  (if (not dragonruby-stargate--active-session)
    (when (called-interactively-p 'interactive)
      (error "No active Stargate session"))
  
  (let* ((index dragonruby-stargate--session-index)
         (branches (cdr (assoc "branches" index)))
         (moments (cdr (assoc "moments" index)))
         (buffer (get-buffer-create "*Stargate Timeline*")))
    
    (when index
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (was-at-end (eobp))
              (pos (point)))
          (erase-buffer)
          (insert (propertize " 🌌 STARGATE: THE BRANCH FOREST\n" 'face 'font-lock-keyword-face))
          (insert (propertize " ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n" 'face 'shadow))
          (insert (propertize " [RET/Click] Jump  [f] Fork Branch  [g] Refresh  [q] Quit\n\n" 'face 'italic))
          
          ;; Start rendering from "prime"
          (dragonruby-stargate-timeline--render-branch "prime" 0 branches moments)
          
          ;; Navigation logic: auto-scroll if we were at the end, else preserve position
          (let ((win (get-buffer-window buffer)))
            (when win
              (if was-at-end
                  (with-selected-window win
                    (goto-char (point-max)))
                (set-window-point win (if (= pos 1) (point-min) pos)))))
          
          (stargate-timeline-mode)
          (when (called-interactively-p 'interactive)
            (message "🌌 Stargate: Timeline rendered from Hash Vault."))))
      (unless (get-buffer-window buffer)
        (display-buffer buffer))))))

(defun dragonruby-stargate-timeline--render-branch (branch-id indent branches-hash moments-hash)
  "Recursively render BRANCH-ID with INDENT using Hash Tables and Local Maps."
  (let* ((prefix (make-string (* indent 2) ?\s))
         (index dragonruby-stargate--session-index)
         (bmaps (cdr (assoc "branch-maps" index)))
         (branch-addresses (gethash branch-id bmaps)))
    
    ;; Render Branch Header
    (insert prefix (propertize (format " 🌿 Branch: %s" branch-id) 
                               'face 'dragonruby-stargate-branch-face) "\n")
    
    ;; Render moments for this branch (Fast path using local map)
    (let ((branch-moments '()))
      (dolist (addr branch-addresses)
        (let ((meta (gethash addr moments-hash)))
          (when meta
            (push (cons addr meta) branch-moments))))
      
      (setq branch-moments 
            (sort branch-moments 
                  (lambda (a b)
                    (< (string-to-number (nth 1 (split-string (car a) "@")))
                       (string-to-number (nth 1 (split-string (car b) "@")))))))
      
      (dolist (m branch-moments)
        (let* ((address (car m))
               (frame (nth 1 (split-string address "@")))
               (meta (cdr m))
               (hash (cdr (assoc "hash" meta)))
               (seed (cdr (assoc "seed" meta)))
               (moment-type (cdr (assoc "moment_type" meta)))
               (is-current (string= address dragonruby-stargate-timeline--last-jump))
               (state-char (if is-current "●" "○"))
               (type-char (cond 
                           ((string= moment-type "input") "⌨️ ")
                           (t "🌿 ")))
               (display-text (format " %s %sFrame %s" state-char type-char frame)))
          (insert prefix "   " 
                  (propertize display-text
                              'face (if is-current 'dragonruby-stargate-current-face 'dragonruby-stargate-frame-face)
                              'mouse-face 'highlight
                              'address address
                              'seed seed
                              'help-echo (format "Click to jump to %s\nType: %s\nHash: %s\nSeed: %s" 
                                                 address moment-type hash seed)
                              'keymap (let ((map (make-sparse-keymap)))
                                        (define-key map [mouse-1] #'dragonruby-stargate-timeline-jump-at-point)
                                        (define-key map (kbd "RET") #'dragonruby-stargate-timeline-jump-at-point)
                                        map))
                  "\n"))))
    
    ;; Find children branches
    (maphash (lambda (id data)
               (let ((parent (cdr (assoc "parent" data)))
                     (div (cdr (assoc "divergence" data))))
                 (when (string= parent branch-id)
                   (insert prefix (propertize "   │\n" 'face 'shadow))
                   (insert prefix (propertize (format "   └─ ⚡ Divergence at frame %s\n" div) 'face 'dragonruby-stargate-divergence-face))
                   (dragonruby-stargate-timeline--render-branch id (+ indent 2) branches-hash moments-hash))))
             branches-hash)))

(defun dragonruby-stargate-timeline-jump-at-point ()
  "Jump to the moment at the current point."
  (interactive)
  (let ((address (get-text-property (point) 'address)))
    (if address
        (dragonruby-stargate-timeline-scrub address)
      (message "No moment at point"))))

(defun dragonruby-stargate-timeline-scrub (address)
  "Jump the simulation to a specific moment ADDRESS (branch@frame)."
  (interactive "sJump to address (branch@frame): ")
  (unless dragonruby-stargate--active-session
    (error "No active Stargate session"))
  
  (let* ((parts (split-string address "@"))
         (branch (nth 0 parts))
         (frame (string-to-number (nth 1 parts)))
         (index dragonruby-stargate--session-index)
         (moments (cdr (assoc "moments" index)))
         (moment (gethash address moments)))
    
    (if moment
        (let* ((hash (cdr (assoc "hash" moment)))
               (seed (cdr (assoc "seed" moment))))
          (setq dragonruby-stargate-timeline--last-jump address)
          (message "⏪ Stargate: Requesting restoration of %s (Hash: %s)..." address hash)
          ;; Michael's Optimization: Send just metadata; DR will load from its native disk.
          (dragonruby-stargate-bridge-send-code 
           (format "Stargate::Clock.restore_moment(%S, %s, %S, %s)" 
                   branch frame hash seed))
          ;; Refresh to update highlights
          (dragonruby-stargate-timeline))
      (error "Moment %s not found in Session Index" address))))
(defun dragonruby-stargate-timeline-fork ()
  "Fork a new branch from the moment at point."
  (interactive)
  (let ((address (get-text-property (point) 'address)))
    (if address
        (let* ((parts (split-string address "@"))
               (parent-id (car parts))
               (frame (string-to-number (cadr parts))))
          (message "🌱 Stargate: Forking new branch from %s@%d..." parent-id frame)
          (dragonruby-stargate-bridge-send-code 
           (format "Stargate::Clock.branch!(%d, %S)" frame parent-id))
          ;; Refresh slightly later to allow the bridge to catch the new branch packet
          (run-with-timer 0.8 nil #'dragonruby-stargate-timeline))
      (message "No moment at point to fork from."))))

(define-derived-mode stargate-timeline-mode special-mode "Stargate-Timeline"
  "Major mode for visualizing the Stargate Branch Forest."
  (setq cursor-type nil)
  (setq buffer-read-only t))

(define-key stargate-timeline-mode-map (kbd "g") 'dragonruby-stargate-timeline)
(define-key stargate-timeline-mode-map (kbd "j") 'dragonruby-stargate-timeline-scrub)
(define-key stargate-timeline-mode-map (kbd "f") 'dragonruby-stargate-timeline-fork)

;; --- AUTO REFRESH ---

(defun dragonruby-stargate-timeline-trigger-refresh (&rest _)
  "Trigger a debounced refresh of the timeline buffer.
Only refreshes if the buffer is actually visible to the user."
  (let ((buffer (get-buffer "*Stargate Timeline*")))
    (when (and buffer (get-buffer-window buffer))
      (when dragonruby-stargate-timeline--refresh-timer
        (cancel-timer dragonruby-stargate-timeline--refresh-timer))
      (setq dragonruby-stargate-timeline--refresh-timer
            (run-with-timer 0.8 nil ;; Increased delay for stability
                            #'dragonruby-stargate-timeline)))))

(add-hook 'dragonruby-stargate-session-updated-hook #'dragonruby-stargate-timeline-trigger-refresh)

(provide 'dragonruby-stargate-timeline)
;;; dragonruby-stargate-timeline.el ends here
