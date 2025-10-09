;;; project-claude.el --- A project.el plugin -*- lexical-binding: t; -*-

;; Copyright (C) 2025 dickmao
;;
;; Author: dickmao
;; Version: 0.0.1
;; URL: https://github.com/dickmao/project-claude
;; Package-Requires: ((vterm "0.0.4"))

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

(require 'project)
(require 'vterm)

(defcustom project-claude-invocation "npx @anthropic-ai/claude-code@latest"
  "Command line shell invocation."
  :group 'project-claude
  :type 'string)

;;;###autoload
(defun project-claude ()
  (interactive)
  (when-let ((normalize (lambda (dir) (expand-file-name (file-name-as-directory dir))))
	     (proj (if (fboundp 'project-most-recent-project)
                       (funcall 'project-most-recent-project)
                     (project-current)))
	     (dir (project-root proj)))
    (if-let ((extant (seq-find
		      (lambda (b)
			(with-current-buffer b
			  (and vterm--term
			       (equal (funcall normalize default-directory)
				      (funcall normalize dir)))))
		      (buffer-list))))
        (pop-to-buffer extant '((display-buffer-use-some-window) . ((some-window . mru))))
      (let ((default-directory dir)
	    (vterm-shell (format "/bin/sh -c '%s'" project-claude-invocation)))
	(vterm-other-window (format "*claude-%s*" (project-name proj)))))))

(provide 'project-claude)
;;; project-claude.el ends here
