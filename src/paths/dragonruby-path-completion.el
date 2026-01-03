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
             ;; Always provide all candidates to allow total project visibility
             (candidates (dragonruby--collect-project-files nil)))
        (list start end candidates
              :exclusive 'no
              :company-kind (lambda (_) 'file)
              :annotation-function (lambda (c) 
                                     (let ((ext (file-name-extension c)))
                                       (format " [%s]" (or ext "file")))))))))

(defun dragonruby--path-context ()
  "Determine if point is in a path context.
Returns (START END TYPE) or nil."
  (let ((p (syntax-ppss)))
    (when (nth 3 p) ; Inside string
      (let* ((string-start (nth 8 p))
             (start (1+ string-start)) ; Just after the opening quote
             (end (point))
             (line-start (line-beginning-position))
             (before-string (buffer-substring-no-properties line-start string-start))
             (type (cond
                    ((string-match-p "\\(require\\|require_relative\\|load\\|load_script\\)" before-string) 'ruby)
                    ((string-match-p "\\(read_file\\|parse_json_file\\|parse_json\\|parse_xml_file\\)" before-string) 'data)
                    (t 'data))))
        (list start end type)))))

(provide 'dragonruby-path-completion)
