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

;; just suggest this in README
;;   (add-to-list 'project-switch-commands '(project-claude "Claude" ?c))

(defun project-claude ()
  (interactive)
  (let* ((proj (project-most-recent-project))
	 (dir (project-root proj)))
    (if-let ((extant (seq-find
		      (lambda (b)
			(with-current-buffer b
			  (and vterm--term
			       (equal (expand-file-name default-directory)
				      (expand-file-name dir)))))
		      (buffer-list))))
	(pop-to-buffer-same-window extant)
      (let ((default-directory dir))
	(with-current-buffer (vterm (format "*claude-%s*"
					    (project-name proj)))
	  (vterm-send-string "claude")
	  (vterm-send-key "<return>"))))))

(provide 'project-claude)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; project-claude.el ends here
