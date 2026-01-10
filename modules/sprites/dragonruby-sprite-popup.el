;;; dragonruby-sprite-popup.el --- Visual popup for sprite previews -*- lexical-binding: t; -*-

(require 'dragonruby-core)

;; --- STATE MACHINE -------------------------------------------------
;;
;; Popup lifecycle:
;;
;;   [Idle] --(hover over sprite)--> [Triggered]
;;     |                                   |
;;     |                            (0.2s idle timer)
;;     |                                   v
;;     |                             [Showing]
;;     |                                   |
;;     |                           (0.25s monitor)
;;     |                                   v
;;     +<--(mouse leaves sprite/popup)--[Cleanup]
;;
;; State variables (all buffer-local):
;;   - `trigger-timer`: Set in Triggered, cleared in Showing/Cleanup
;;   - `timer`: Set in Showing (monitor), cleared in Cleanup
;;   - `frame`: Created in Showing, destroyed in Cleanup
;;   - `current-path`: Set in Showing, cleared in Cleanup
;;
;; Invariants:
;;   - Only ONE popup visible per buffer at a time
;;   - Frame is always child of parent frame (coordinate space)
;;   - Cleanup is idempotent (safe to call multiple times)
;;
;; --- STATE ----------------------------------------------------------

(defvar-local dragonruby--sprite-popup-frame nil)
(defvar dragonruby--sprite-popup-buffer " *DragonRuby Sprite Popup*")
(defvar-local dragonruby--sprite-popup-current-path nil)

(defvar-local dragonruby--sprite-popup-timer nil)
(defvar-local dragonruby--sprite-popup-trigger-timer nil)

(defvar-local dragonruby--sprite-popup-face-cookie nil)

;; --- UTILS ----------------------------------------------------------

(defun dragonruby--sprite-popup--graphic-p ()
  (and (display-graphic-p)
       (frame-live-p (selected-frame))))

(defun dragonruby--sprite-popup-get-colors ()
  (list :bg (or (face-background 'default nil t) "#000000")
        :fg (or (face-foreground 'default nil t) "#ffffff")
        :border (or (face-foreground 'font-lock-keyword-face nil t) "#888888")))

(defun dragonruby--sprite-popup--cancel-timer (sym)
  (when (and (boundp sym) (timerp (symbol-value sym)))
    (cancel-timer (symbol-value sym)))
  (when (boundp sym) (set sym nil)))

;; --- CLEANUP --------------------------------------------------------

(defun dragonruby--sprite-popup-cleanup ()
  (dragonruby--sprite-popup--cancel-timer 'dragonruby--sprite-popup-timer)
  (dragonruby--sprite-popup--cancel-timer 'dragonruby--sprite-popup-trigger-timer)

  (setq dragonruby--sprite-popup-current-path nil)

  (when (and dragonruby--sprite-popup-face-cookie
             (buffer-live-p (get-buffer dragonruby--sprite-popup-buffer)))
    (with-current-buffer dragonruby--sprite-popup-buffer
      (face-remap-remove-relative dragonruby--sprite-popup-face-cookie))
    (setq dragonruby--sprite-popup-face-cookie nil))

  (when (frame-live-p dragonruby--sprite-popup-frame)
    (delete-frame dragonruby--sprite-popup-frame t))
  (setq dragonruby--sprite-popup-frame nil))

;; --- MONITOR --------------------------------------------------------

(defun dragonruby--sprite-popup-monitor ()
  (unless (dragonruby--sprite-popup--graphic-p)
    (dragonruby--sprite-popup-cleanup))

  (let* ((mpos (mouse-pixel-position))
         (frame (and (consp mpos) (car mpos)))
         (x (cadr mpos))
         (y (let ((r (cddr mpos))) (and (consp r) (car r))))
         (is-over nil))
    (ignore-errors
      (when (and (frame-live-p frame) (numberp x) (numberp y))
        (let* ((posn (posn-at-x-y x y frame))
               (window (and posn (posn-window posn)))
               (point (and posn (posn-point posn))))
          (when (and (windowp window) (integerp point))
            (with-current-buffer (window-buffer window)
              (dolist (ov (overlays-at point))
                (when (overlay-get ov 'dragonruby-sprite)
                  (setq is-over t))))))))
    (when (eq frame dragonruby--sprite-popup-frame)
      (setq is-over t))

    (if is-over
        (setq dragonruby--sprite-popup-timer
              (run-with-timer 0.25 nil #'dragonruby--sprite-popup-monitor))
      (dragonruby--sprite-popup-cleanup))))

;; --- SHOW -----------------------------------------------------------

(defun dragonruby--sprite-popup-show (path)
  (message ">>> SHOW CALLED: path=%s graphic-p=%s" path (dragonruby--sprite-popup--graphic-p))
  
  (unless (dragonruby--sprite-popup--graphic-p)
    (message ">>> SHOW ABORT: Not graphic-p")
    (dragonruby--sprite-popup-cleanup))

  (message ">>> SHOW: Checking guards... exists=%s"
           (file-exists-p path))
  
  ;; SIMPLIFIED: Always render - let monitor handle cleanup
  ;; Previous compound invariant was too strict (frame "live" but invisible)
  (when (and path (file-exists-p path))
    (message ">>> SHOW: Guards PASSED, rendering popup...")
    (setq dragonruby--sprite-popup-current-path path)

    (let* ((buf (get-buffer-create dragonruby--sprite-popup-buffer))
           (colors (dragonruby--sprite-popup-get-colors))
           (img (ignore-errors
                  (create-image path (dragonruby--get-image-type path) nil
                                :max-width 200 :max-height 200
                                :background "none" :transform-smoothing t)))
           (attrs (ignore-errors (file-attributes path)))
           (size (if attrs (file-size-human-readable (file-attribute-size attrs)) "?"))
           (ext (upcase (or (file-name-extension path) "IMG")))
           (dims (ignore-errors
                   (when img (image-size img t))))
           (w (if (and dims (numberp (car dims))) (truncate (car dims)) 0))
           (h (if (and dims (numberp (cdr dims))) (truncate (cdr dims)) 0))
           (meta (format "📐 %dx%d | 📏 %s | 🖼️ %s" w h size ext)))

      (message ">>> IMG CREATED: img=%s dims=%s" 
               (if img "SUCCESS" "FAILED") dims)

      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (setq-local cursor-type nil)
          (when dragonruby--sprite-popup-face-cookie
            (face-remap-remove-relative dragonruby--sprite-popup-face-cookie))
          (setq dragonruby--sprite-popup-face-cookie
                (face-remap-add-relative
                 'default `(:background ,(plist-get colors :bg)
                             :foreground ,(plist-get colors :fg))))
          (insert "\n  ")
          (if img
              (insert-image img)
            ;; Fallback: Show clear error message
            (insert (propertize "⚠️ IMAGEN NO DISPONIBLE\n"
                               'face '(:foreground "orange" :weight bold))
                    (propertize (format "\nPath: %s\n" (file-name-nondirectory path))
                               'face '(:foreground "gray"))
                    (propertize "\n¿Emacs compilado con soporte de imágenes?"
                               'face '(:foreground "gray" :slant italic))))
          (insert "\n\n  ")
          (insert (propertize meta 'face '(:weight bold)))
          (insert "\n")
          ;; DEBUG: Uncomment to trace buffer filling
          ;; (message ">>> BUFFER FILLED: size=%d has-image=%s" 
          ;;          (buffer-size) (if img "YES" "NO"))
          (set-buffer-modified-p nil)
          (setq buffer-read-only t)))

      (let* ((mpos (mouse-pixel-position))
             (parent (or (car-safe mpos) (selected-frame)))
             (x (or (cadr mpos) 0))
             (y (let ((r (cddr mpos))) (or (and (consp r) (car r)) 0)))
             (left (+ x 30))
             (top  (+ y 20)))

        (unless (frame-live-p dragonruby--sprite-popup-frame)
          (setq dragonruby--sprite-popup-frame
                (make-frame
                 `((parent-frame . ,parent)
                   (minibuffer . nil)
                   (unsplittable . t)
                   (no-accept-focus . t)
                   (no-focus-on-map . t)
                   (child-frame-border-width . 1)
                   (border-color . ,(plist-get colors :border))
                   (internal-border-width . 10)
                   (vertical-scroll-bars . nil)
                   (horizontal-scroll-bars . nil)
                   (menu-bar-lines . 0)
                   (tool-bar-lines . 0)
                   (tab-bar-lines . 0)
                   (no-other-frame . t)
                   (cursor-type . nil)
                   (undecorated . t)
                   (override-redirect . t)))))

        (when (frame-live-p dragonruby--sprite-popup-frame)
          (modify-frame-parameters
           dragonruby--sprite-popup-frame
           `((buffer . ,buf)
             (left . ,left)
             (top . ,top)
             (visibility . t)))))

      (dragonruby--sprite-popup--cancel-timer 'dragonruby--sprite-popup-timer)
      (setq dragonruby--sprite-popup-timer
            (run-with-timer 0.6 nil #'dragonruby--sprite-popup-monitor)))))

;; --- TRIGGER --------------------------------------------------------

(defun dragonruby--sprite-popup-trigger (path)
  ;; DEBUG: (message ">>> TRIGGER CALLED for: %s" path)
  (dragonruby--sprite-popup--cancel-timer 'dragonruby--sprite-popup-trigger-timer)
  (setq dragonruby--sprite-popup-trigger-timer
        (run-with-idle-timer 0.2 nil #'dragonruby--sprite-popup-show path))
  ;; DEBUG: (message ">>> TRIGGER: Timer scheduled (0.2s)")
  )

(provide 'dragonruby-sprite-popup)
;;; dragonruby-sprite-popup.el ends here
