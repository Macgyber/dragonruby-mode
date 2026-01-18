;;; dragonruby-path-completion.el --- Contextual CAPF for paths -*- lexical-binding: t; -*-

(require 'dragonruby-core)
(require 'dragonruby-path-fs)

(defun dragonruby-path-completion-at-point ()
  "CAPF for DragonRuby paths inside strings and require statements.
Low priority, contextual, and manual (triggered by user)."
  (let ((context (dragonruby--path-context)))
    (when context
      (let* ((start (nth 0 context))
             (end (nth 1 context))
             (type (nth 2 context))
             ;; If the string already starts with "sprites/" or "spr/", force sprite type
             (content (buffer-substring-no-properties start end))
             (effective-type (if (or (string-prefix-p "sprites" content)
                                      (string-prefix-p "spr" content))
                                 'sprite
                               type))
             (candidates (dragonruby--collect-project-files effective-type)))
        (list start end candidates
              :exclusive 'no
              :company-kind (lambda (_) 'file)
              :annotation-function (lambda (c) 
                                     (let ((ext (file-name-extension c)))
                                       (format " [%s]" (or ext "file"))))
              :exit-function (lambda (_str status)
                               (when (memq status '(finished sole))
                                 ;; Aggressively jump out of the string
                                 (unless (search-forward "\"" (line-end-position) t)
                                   (let ((p (syntax-ppss)))
                                     (when (nth 3 p) ;; Still inside string
                                       (goto-char (nth 8 p)) ;; Back to quote start
                                       (ignore-errors (forward-sexp))))))))))))

(defun dragonruby--path-context ()
  "Determine if point is in a path context.
Returns (START END TYPE) or nil."
  (let ((p (syntax-ppss)))
    (when (nth 3 p) ; Inside string
      (let* ((string-start (nth 8 p))
             (start (1+ string-start)) ; Just after the opening quote
             (end (save-excursion
                    (goto-char string-start)
                    (forward-sexp)
                    (1- (point)))) ; Just before the closing quote
             (line-start (line-beginning-position))
             (content (buffer-substring-no-properties start end))
             (before-string (buffer-substring-no-properties line-start string-start))
             (type (cond
                    ((string-match-p "\\(require\\|require_relative\\|load\\|load_script\\)" before-string) 'ruby)
                    ((string-match-p "\\(read_file\\|write_file\\|parse_json_file\\|parse_json\\|parse_xml_file\\)" before-string) 'data)
                    ((or (string-match-p "\\(sprites\\|primitives\\|labels\\|path:\\)" before-string)
                         (string-prefix-p "sprites/" content)
                         (string-prefix-p "spr/" content)
                         (string-prefix-p ".png" content)
                         (string-prefix-p ".jpg" content)
                         (string-prefix-p ".gif" content)
                         (string-prefix-p ".bmp" content)) 'sprite)
                    ((or (string-match-p "\\(sounds\\|audio\\)" before-string)
                         (string-prefix-p "sounds/" content)
                         (string-prefix-p "audio/" content)) 'audio)
                    ((or (string-match-p "\\(fonts\\|font:\\)" before-string)
                         (string-prefix-p "fonts/" content)) 'font)
                    (t nil)))) ; Default: Universal fallback
        (list start end type)))))

(provide 'dragonruby-path-completion)
