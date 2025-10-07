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

(defun project-claude-modify-project-switch-commands ()
  (defvar project-claude-project-switch-command)
  (let ((command '(project-claude "Claude" ?c)))
    (if project-claude-project-switch-command
        (add-to-list 'project-switch-commands command :append)
      (when (member command project-switch-commands)
        (customize-set-variable 'project-switch-commands
                                (delete command project-switch-commands))))))

(defcustom project-claude-project-switch-command t
  "Add to `project-switch-commands'."
  :type 'symbol
  :set (lambda (symbol value)
         (set-default symbol value)
         (project-claude-modify-project-switch-commands))
  :group 'project-claude)

(defcustom project-claude-invocation nil
  "Command line shell invocation."
  :group 'project-claude
  :type '(choice (const :tag "None" nil)
          (string :tag "Value")))

;;;###autoload
(defun project-claude ()
  (interactive)
  (let* ((proj (if (fboundp 'project-most-recent-project)
                   (funcall 'project-most-recent-project)
                 (project-current)))
	 (dir (project-root proj)))
    (if-let ((extant (seq-find
		      (lambda (b)
			(with-current-buffer b
			  (and vterm--term
			       (equal (expand-file-name default-directory)
				      (expand-file-name dir)))))
		      (buffer-list))))
	(pop-to-buffer extant)
      (let ((default-directory dir))
	(with-current-buffer (vterm (format "*claude-%s*"
					    (project-name proj)))
	  (vterm-send-string (or project-claude-invocation "claude"))
	  (vterm-send-key "<return>")
          (current-buffer))))))

(project-claude-modify-project-switch-commands)

(provide 'project-claude)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; project-claude.el ends here
