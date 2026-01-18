;;; dragonruby-core-test.el --- Basic tests for DragonRuby mode core -*- lexical-binding: t; -*-

(require 'ert)
(require 'dragonruby-mode)

(ert-deftest dragonruby-mode-can-load ()
  "Test if dragonruby-mode can be loaded without errors."
  (should (featurep 'dragonruby-mode)))

(ert-deftest dragonruby-kernel-boot-test ()
  "Test if the kernel registers modules correctly."
  (with-temp-buffer
    (setq-local buffer-file-name "test_project/app/main.rb")
    (dragonruby-mode 1)
    (let ((active (cl-loop for k being the hash-keys in (dragonruby--active-hash) collect k)))
      (should (memq 'core active)))
    (dragonruby-mode -1)))

(provide 'dragonruby-core-test)
