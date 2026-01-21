;;; dragonruby-stargate-scrubber.el --- Stargate Temporal UI Checker -*- lexical-binding: t -*-

(require 'dragonruby-stargate-timeline)

(defface dragonruby-stargate-branch-header-face '((t :inherit font-lock-function-name-face :weight bold)) "Face for branch identifiers.")
(defface dragonruby-stargate-moment-face '((t :foreground "#AAAAAA")) "Face for standard recorded moments.")
(defface dragonruby-stargate-active-moment-face '((t :inherit highlight :box (:line-width 1 :color "green"))) "Face for active moment.")
(defface dragonruby-stargate-pending-moment-face '((t :inherit highlight :box (:line-width 1 :color "yellow"))) "Face for jump target.")
(defface dragonruby-stargate-diverge-marker-face '((t :inherit error :weight bold)) "Face for [!] markers.")

(defun dragonruby-stargate-scrubber ()
  "Render the Stargate Scrubber (Analytical Timeline)."
  (interactive)
  (let* ((index dragonruby-stargate--session-index)
         (branches (cdr (assoc "branches" index)))
         (moments (cdr (assoc "moments" index)))
         (buffer (get-buffer-create "*Stargate Timeline*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert " [LOG] STARGATE_SCRUBBER v1.0.1\n ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
        (insert " [RET] Jump Intent   [F] Fork Branch   [G] Refresh   [Q] Close\n\n")
        (dragonruby-stargate-scrubber--render-node "prime" 0 branches moments)
        (stargate-scrubber-mode)
        (goto-char (point-max))))
    (display-buffer buffer)))

(defun dragonruby-stargate-scrubber--render-node (branch-id indent branches moments)
  (let* ((prefix (make-string (* indent 2) ?\s))
         (addresses (gethash branch-id (cdr (assoc "branch-maps" dragonruby-stargate--session-index)))))
    (insert prefix "[-] " (propertize (format "BRANCH: %s" branch-id) 'face 'dragonruby-stargate-branch-header-face) "\n")
    (dolist (addr addresses)
      (let* ((meta (gethash addr moments))
             (frame (nth 1 (split-string addr "@")))
             (is-active (string= addr dragonruby-stargate--active-address))
             (face (if is-active 'dragonruby-stargate-active-moment-face 'dragonruby-stargate-moment-face)))
        (insert prefix "  " (propertize (format " [ ] FRAME_%s" frame) 'face face 'address addr 
                                       'keymap (let ((m (make-sparse-keymap))) (define-key m (kbd "RET") #'dragonruby-stargate-timeline-jump-at-point) m)) "\n")))
    (maphash (lambda (id data) (when (string= (cdr (assoc "parent" data)) branch-id)
                                 (insert prefix "  +-- " (propertize "[!]" 'face 'dragonruby-stargate-diverge-marker-face) "\n")
                                 (dragonruby-stargate-scrubber--render-node id (+ indent 2) branches moments))) branches)))

(define-derived-mode stargate-scrubber-mode special-mode "Stargate-Scrubber"
  "Major mode for Stargate Temporal Scrubber."
  (setq cursor-type nil)
  (define-key stargate-scrubber-mode-map (kbd "g") 'dragonruby-stargate-scrubber)
  (define-key stargate-scrubber-mode-map (kbd "f") 'dragonruby-stargate-timeline-fork))

(add-hook 'dragonruby-stargate-session-updated-hook #'dragonruby-stargate-scrubber)
(provide 'dragonruby-stargate-scrubber)
