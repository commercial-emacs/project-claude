;;; test-project-@PROVIDER@-generated.el --- do not edit -*- lexical-binding: t; -*-

(require 'ert)
(require 'project-@PROVIDER@)

(ert-deftest @PROVIDER@-basic ()
  (let* (ghostty-vt-mode-hook ;neutralize project-gemini/clear-on-startup
	 (project-@PROVIDER@/invocation (format "echo foo && sleep 20"))
	 (buf (project-@PROVIDER@ :no-solicit t)))
    (should (eq buf (current-buffer)))
    (should (project-@PROVIDER@//wait-for (regexp-quote "foo")))
    (let (kill-buffer-query-functions)
      (kill-buffer buf))))

(ert-deftest @PROVIDER@-current-prefix-arg-insert-file-ref ()
  "File reference is relative path to emacs-ghostty subdir."
  (cl-letf* ((ghostty-vt-mode-hook) ;neutralize project-gemini/clear-on-startup
	     ((symbol-function 'project-remember-project) #'identity)
	     (project-@PROVIDER@/invocation "/bin/sh")
	     (project-@PROVIDER@/prompt-regex "$")
	     (parent-dir default-directory)
	     (parent-project (project-current))
	     (default-directory (expand-file-name "emacs-ghostty"))
	     (current-project (project-current)))
    (should-not (equal parent-project current-project))
    (project-@PROVIDER@/ensure-ready
     (should (equal (project-current) current-project)))
    (let* ((default-directory parent-dir)
	   (b (find-file "test-template.el")))
      (should (equal (current-buffer) b))
      (should (equal parent-project (project-current)))
      (let ((current-prefix-arg '(4)))
	(call-interactively #'project-@PROVIDER@/insert-file-ref)
	(goto-char (point-min))
	(prin1 (buffer-string))
	(should (re-search-forward (regexp-quote "@../test-template.el:1") nil t)))
      (kill-buffer b))))

(provide 'test-project-@PROVIDER@-generated)
;;; test-project-@PROVIDER@-generated.el ends here
