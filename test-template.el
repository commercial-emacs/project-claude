;;; test-project-@PROVIDER@-generated.el --- do not edit -*- lexical-binding: t; -*-

(require 'ert)
(require 'project-@PROVIDER@)

(ert-deftest @PROVIDER@-basic ()
  (let* (vterm-mode-hook ;neutralize project-gemini/clear-on-startup
	 (project-@PROVIDER@/invocation (format "echo foo && sleep 20"))
	 (buf (project-@PROVIDER@ :no-solicit t)))
    (should (eq buf (current-buffer)))
    (should (project-@PROVIDER@//wait-for (regexp-quote "foo")))))

(provide 'test-project-@PROVIDER@-generated)
;;; test-project-@PROVIDER@-generated.el ends here
