;;; integration_test.el --- Global integration test for DragonRuby-mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'dragonruby-mode)

(ert-deftest dragonruby-full-integration-test ()
  "Test the full boot cycle of dragonruby-mode."
  (with-temp-buffer
    (setq-local buffer-file-name "test_project/app/main.rb")
    (dragonruby-mode 1)
    (should dragonruby-mode)
    (dragonruby-mode -1)
    (should-not dragonruby-mode)))

(provide 'integration_test)
