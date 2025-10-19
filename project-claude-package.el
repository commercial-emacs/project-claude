;;; project-claude-package.el --- because package.el sucks ass  -*- lexical-binding:t -*-

(require 'package)
(require 'project)

(defsubst project-claude-package-where ()
  (directory-file-name (expand-file-name (project-root (project-current)))))

(defsubst project-claude-package-desc ()
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "project-claude.el" (project-claude-package-where)))
    (package-buffer-info)))

(defun project-claude-package-name ()
  (concat "project-claude-" (package-version-join
				(package-desc-version
				 (project-claude-package-desc)))))

(defun project-claude-package-inception ()
  "To get a -pkg.el file, you need to run `package-unpack'.
To run `package-unpack', you need a -pkg.el."
  (let ((pkg-desc (project-claude-package-desc))
	(pkg-dir (expand-file-name (project-claude-package-name)
				   (project-claude-package-where))))
    (ignore-errors (delete-directory pkg-dir t))
    (make-directory pkg-dir t)
    (dolist (el (split-string "project-claude.el project-gemini.el project-claude-generated.el project-gemini-generated.el"))
      (copy-file (expand-file-name el (project-claude-package-where))
		 (expand-file-name el pkg-dir)))
    (package--make-autoloads-and-stuff pkg-desc pkg-dir)))
