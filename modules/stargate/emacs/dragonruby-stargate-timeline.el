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

(defun dragonruby-stargate-timeline-render ()
  "Render the Branch Forest visualization for the current session."
  (interactive)
  (unless dragonruby-stargate--active-session
    (unless (called-interactively-p 'interactive)
      (return nil))
    (error "No active Stargate session"))
  
  (let* ((index-file (expand-file-name "session.json" dragonruby-stargate--active-session))
         (json-object-type 'alist)
         (json-key-type 'string)
         (index (condition-case nil (json-read-file index-file) (error nil)))
         (branches (cdr (assoc "branches" index)))
         (moments (cdr (assoc "moments" index)))
         (buffer (get-buffer-create "*Stargate Timeline*")))
    
    (when index
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (pos (point)))
          (erase-buffer)
          (insert (propertize " 🌌 STARGATE: THE BRANCH FOREST\n" 'face 'info))
          (insert (propertize " ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n" 'face 'shadow))
          
          ;; Start rendering from "prime"
          (dragonruby-stargate-timeline--render-branch "prime" 0 branches moments)
          
          (goto-char (if (= pos 1) (point-min) pos))
          (stargate-timeline-mode)))
      (unless (get-buffer-window buffer)
        (display-buffer buffer)))))

(defun dragonruby-stargate-timeline--render-branch (branch-id indent branches moments)
  "Recursively render BRANCH-ID with INDENT."
  (let ((prefix (make-string (* indent 2) ?\s))
        (is-prime (string= branch-id "prime")))
    
    ;; Render Branch Header
    (insert prefix (propertize (format " 🌿 Branch: %s" branch-id) 
                               'face 'dragonruby-stargate-branch-face 
                               'help-echo (format "Branch ID: %s" branch-id)) "\n")
    
    ;; Render Moments for this branch
    (let ((branch-moments '()))
      (dolist (m moments)
        (when (string-prefix-p (concat branch-id "@") (car m))
          (push m branch-moments)))
      
      ;; Sort moments by frame number
      (setq branch-moments 
            (sort branch-moments 
                  (lambda (a b)
                    (< (string-to-number (nth 1 (split-string (car a) "@")))
                       (string-to-number (nth 1 (split-string (car b) "@")))))))
      
      (dolist (m branch-moments)
        (let* ((address (car m))
               (frame (nth 1 (split-string address "@")))
               (hash (cdr (assoc "hash" (cdr m))))
               (is-current (string= address dragonruby-stargate-timeline--last-jump))
               (char (if is-current " ● " " ○ ")))
          (insert prefix "   " 
                  (propertize (concat char "Frame " frame)
                              'face (if is-current 'dragonruby-stargate-current-face 'dragonruby-stargate-frame-face)
                              'mouse-face 'highlight
                              'address address
                              'help-echo (format "Click to jump to %s\nHash: %s" address hash)
                              'keymap (let ((map (make-sparse-keymap)))
                                        (define-key map [mouse-1] #'dragonruby-stargate-timeline-jump-at-point)
                                        (define-key map (kbd "RET") #'dragonruby-stargate-timeline-jump-at-point)
                                        map))
                  "\n")))
    
    ;; Find children branches
    (dolist (b branches)
      (let* ((id (car b))
             (parent (cdr (assoc "parent" (cdr b))))
             (div (cdr (assoc "divergence" (cdr b)))))
        (when (string= parent branch-id)
          (insert prefix (propertize "   │\n" 'face 'shadow))
          (insert prefix (propertize (format "   └─ ⚡ Divergence at frame %s\n" div) 'face 'dragonruby-stargate-divergence-face))
          (dragonruby-stargate-timeline--render-branch id (+ indent 2) branches moments)))))))

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
         (index-file (expand-file-name "session.json" dragonruby-stargate--active-session))
         (json-object-type 'alist)
         (json-key-type 'string)
         (index (json-read-file index-file))
         (moments (cdr (assoc "moments" index)))
         (moment (cdr (assoc address moments))))
    
    (if moment
        (let* ((hash (cdr (assoc "hash" moment)))
               (data (dragonruby-stargate-vault-get hash)))
          (if data
              (progn
                (setq dragonruby-stargate-timeline--last-jump address)
                (message "⏪ Stargate: Restoring state for %s..." address)
                ;; Send the state data and coordinates to the Runtime
                (dragonruby-stargate-bridge-send-code 
                 (format "Stargate::Clock.restore_moment(%S, %s, %S)" 
                         branch frame data))
                ;; Refresh to update highlights
                (dragonruby-stargate-timeline-render))
            (error "State blob %s not found in Vault" hash)))
      (error "Moment %s not found in Session Index" address))))

(define-derived-mode stargate-timeline-mode special-mode "Stargate-Timeline"
  "Major mode for visualizing the Stargate Branch Forest."
  (setq cursor-type nil)
  (setq buffer-read-only t))

(define-key stargate-timeline-mode-map (kbd "g") 'dragonruby-stargate-timeline-render)
(define-key stargate-timeline-mode-map (kbd "j") 'dragonruby-stargate-timeline-scrub)

;; --- AUTO REFRESH ---

(defun dragonruby-stargate-timeline-trigger-refresh (&rest _)
  "Trigger a debounced refresh of the timeline buffer."
  (when (get-buffer "*Stargate Timeline*")
    (when dragonruby-stargate-timeline--refresh-timer
      (cancel-timer dragonruby-stargate-timeline--refresh-timer))
    (setq dragonruby-stargate-timeline--refresh-timer
          (run-with-timer dragonruby-stargate-timeline-refresh-delay nil
                          #'dragonruby-stargate-timeline-render))))

(add-hook 'dragonruby-stargate-bridge-event-hook #'dragonruby-stargate-timeline-trigger-refresh)

(provide 'dragonruby-stargate-timeline)
;;; dragonruby-stargate-timeline.el ends here
