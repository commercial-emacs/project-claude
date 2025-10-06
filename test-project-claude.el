;;; test-project-claude.el --- Tests for project-claude -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2020 by Lukas Fürmetz & Contributors

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Tests for project-claude.

;;; Code:

(require 'ert)
(require 'vterm)

(defvar-local test-project-claude/prompt nil)

(defmacro test-project-claude/with-session (&rest body)
  (declare (indent defun))
  `(let ((b (vterm--get-buffer nil))
	 (wait-refresh (lambda (&rest _args)
			 (accept-process-output vterm--process 0.05 nil t))))
     (unwind-protect
	 (with-current-buffer b
	   (dolist (f '(vterm-send vterm-send-key vterm-send-string))
	     (add-function :after (symbol-function f) wait-refresh))
	   (should (get-buffer-process (current-buffer)))
	   (cl-loop repeat 100
		    until (not (zerop (current-column)))
		    do (sleep-for 0.2)
		    finally
		    (progn (setq test-project-claude/prompt
				 (buffer-substring-no-properties
				  (line-beginning-position) (point)))
			   (should-not (zerop (length test-project-claude/prompt)))))
	   ,@body)
       (dolist (f '(vterm-send vterm-send-key vterm-send-string))
	 (remove-function (symbol-function f) wait-refresh))
       (let (kill-buffer-query-functions)
	 (kill-buffer b)
	 (cl-loop repeat 100
		  until (not (get-buffer-process b))
		  do (sleep-for 0.2)
		  finally (should-not (get-buffer-process b)))))))

(defsubst test-project-claude/at-prompt ()
  (equal (buffer-substring-no-properties
	  (line-beginning-position) (point))
	 test-project-claude/prompt))

(defsubst test-project-claude/run (command)
  (should (test-project-claude/at-prompt))
  (vterm-send-string command)
  (vterm-send-key "<return>")
  (cl-loop repeat 100
	   until (test-project-claude/at-prompt)
	   do (sleep-for 0.2)
	   finally (should (test-project-claude/at-prompt))))

(ert-deftest basic ()
  (test-project-claude/with-session
    (should (eq major-mode 'vterm-mode))))

(ert-deftest beauty-wrap ()
  (test-project-claude/with-session
    (should (= (window-width) vterm-min-window-width))
    (let* ((x3 (make-string (* 3 (1- (window-width))) ?x)))
      (test-project-claude/run (format "echo %s" x3))
      (save-excursion
	(forward-line -1)
	(let ((pure-text (buffer-substring-no-properties
			  (line-beginning-position) (line-end-position))))
	  (should-not (equal pure-text x3))))
      (call-interactively #'vterm-copy-mode)
      (should (test-project-claude/at-prompt))
      (save-excursion
	(forward-line -1) ;logical
	(should (equal x3 (buffer-substring-no-properties
			   (line-beginning-position) (line-end-position))))))))

(provide 'test-project-claude)
;;; test-project-claude.el ends here
