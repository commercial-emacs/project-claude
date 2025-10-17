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

(defconst project-claude/prompt-regex "─+[^─]*>\\s-+" "Unfortunate.")

(defgroup project-claude nil
  "Integration with Claude Code CLI."
  :group 'tools
  :prefix "project-claude/")

(defcustom project-claude/invocation "npx @anthropic-ai/claude-code@latest"
  "Command line shell invocation."
  :group 'project-claude
  :type 'string)

;;;###autoload
(cl-defun project-claude (&key no-solicit)
  "Returns Claude Code buffer for current project.

Use NO-SOLICIT if wanting to avoid pre-startup questions (as one
would if cold-starting from an in-band query)."
  (interactive)
  (when-let ((proj (if (fboundp 'project-most-recent-project)
                       (funcall 'project-most-recent-project)
                     (project-current)))
	     (default-directory (project-root proj))
	     (buf (get-buffer-create (format "*claude-%s*" (project-name proj)))))
    (if (with-current-buffer buf (and vterm--term (process-live-p vterm--process)))
	(pop-to-buffer buf '((display-buffer-use-some-window) . ((some-window . mru))))
      (let ((vterm-shell
	     (format "/bin/sh -c '%s'"
		     (concat (when no-solicit
			       "DISABLE_TELEMETRY=1 DISABLE_AUTOUPDATER=1 ")
			     project-claude/invocation)))
	    (vterm-buffer-name (buffer-name buf)))
	(vterm-other-window)))))

(defvar project-claude/prompt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'project-claude/prompt-send)
    (define-key map (kbd "C-c C-k") 'project-claude/prompt-cancel)
    map)
  "Keymap for `project-claude/prompt-mode'.")

(define-derived-mode project-claude/prompt-mode text-mode "Claude-Prompt"
  "Major mode for entering prompts to send to Claude Code.
\\{project-claude/prompt-mode-map}"
  (setq header-line-format
        "Enter prompt for Claude Code. C-c C-c to send, C-c C-k to cancel"))

(defun project-claude/prompt-cancel ()
  "Call it off."
  (interactive)
  (quit-window t))

(defun project-claude//wait-for (regex &optional from)
  "Return t on success."
  (save-excursion
    (goto-char (or from (point-min)))
    (cl-loop with success
	     repeat 200
	     until (setq success (save-excursion
				   (goto-char (or from (point-min)))
				   (re-search-forward regex nil t)))
	     do (accept-process-output vterm--process 0.05 nil t)
	     finally return success)))

(defun project-claude/cursor-pos ()
  (save-excursion
    (goto-char (point-max))
    (re-search-backward project-claude/prompt-regex nil t)
    (prop-match-beginning
     (text-property-search-forward
      'font-lock-face t
      (lambda (value prop)
	"What asshole wrote and documented t-p-s-f."
	(and (listp prop)
	     (eq value (plist-get prop :inverse-video))))))))

(defun project-claude/clear-input ()
  "Fraught."
  (interactive)
  (when vterm-copy-mode
    (vterm-copy-mode-done))
  (save-excursion
    (goto-char (point-max))
    (re-search-backward project-claude/prompt-regex nil t)
    (catch 'done
      (let (prev-cursor cursor)
	(while (setq cursor (project-claude/cursor-pos))
	  (when (equal prev-cursor cursor)
	    (throw 'done nil))
	  (setq prev-cursor cursor)
	  (vterm-send-key "b" nil nil '(control))
	  (setq this-command 'vterm-send-key)
	  (accept-process-output vterm--process 0.05 nil t))))
    (goto-char (project-claude/cursor-pos))
    (catch 'done
      (while t
	(when (or (looking-at-p "\\s-+──")
		  (looking-at-p "Try "))
	  (throw 'done nil))
	(vterm-send-key "k" nil nil '(control))
	(setq this-command 'vterm-send-key)
	(accept-process-output vterm--process 0.05 nil t)))))

(defun project-claude/issue-this (what)
  "You can send commands willy-nilly to bash.
But something about Claude Code's input processing interprets a too-soon
RET as M-RET."
  (when (project-claude//wait-for project-claude/prompt-regex)
    (when vterm-copy-mode
      (vterm-copy-mode-done))
    ;;(project-claude/clear-input)
    (let ((start (window-start)))
      (let ((inhibit-read-only t))
	(vterm-send-string what))
      (when (project-claude//wait-for (regexp-quote what) start)
	(let ((inhibit-read-only t))
	  (vterm-send-key "<return>"))
	(setq this-command 'vterm-send-key))) ;for vterm--filter
    ))

(defun project-claude/prompt-send ()
  "Send prompt buffer contents to Claude Code and close prompt buffer."
  (interactive)
  (when-let ((prompt (buffer-substring-no-properties (point-min) (point-max)))
	     (not-empty-p (not (string-empty-p (string-trim prompt)))))
    (quit-window t)
    (let ((buf (project-claude :no-solicit t)))
      (cl-assert (eq buf (current-buffer)))
      (project-claude/issue-this prompt))))

;;;###autoload
(defun project-claude/prompt ()
  "Open a prompt buffer to send queries to Claude Code."
  (interactive)
  (when (> (length (window-list)) 2)
    (delete-other-windows))
  (let ((file-ref (project-claude/file-reference))
	(buf (get-buffer-create "*claude-prompt*")))
    (with-current-buffer buf
      (project-claude/prompt-mode)
      (erase-buffer)
      (when file-ref (insert file-ref " ")))
    (pop-to-buffer buf '((display-buffer-at-bottom)
			 (window-height . 5)))))

(defun project-claude/file-reference ()
  "Construct Claude Code file reference from current position."
  (when-let ((file (buffer-file-name))
             (proj (project-current)))
    (let* ((rel-file (file-relative-name file (project-root proj)))
           (start (line-number-at-pos (if (use-region-p)
                                          (region-beginning)
                                        (point))))
           (end (when (use-region-p)
                  (line-number-at-pos (region-end)))))
      (if end
          (format "@%s:%d-%d" rel-file start end)
        (format "@%s:%d" rel-file start)))))

(eval-when-compile
  (defun escape-quotes-in-docstring (string)
    "Needs to go upstream."
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      ;; Escape grave accents (`) that aren't already escaped
      (while (re-search-forward "\\(?:[^\\]\\|^\\)\\(`\\)" nil t)
        (replace-match "\\\\=`" nil nil nil 1))
      ;; Escape apostrophes (') that aren't already escaped
      (goto-char (point-min))
      (while (re-search-forward "\\(?:[^\\]\\|^\\)\\('\\)" nil t)
        (replace-match "\\\\='" nil nil nil 1))
      (buffer-string)))
  (add-function :filter-return (symbol-function 'internal--format-docstring-line)
		#'escape-quotes-in-docstring))

(define-minor-mode project-claude-mode
  "Minor mode for Claude Code integration."
  :group 'project-claude
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c '") #'project-claude/prompt)
            map))

;;;###autoload
(define-globalized-minor-mode global-project-claude-mode
  project-claude-mode (lambda ()
			(when (derived-mode-p 'prog-mode)
			  (project-claude-mode 1)))
  :group 'project-claude)

(provide 'project-claude)
;;; project-claude.el ends here
