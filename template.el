;;; project-@PROVIDER@-generated.el --- do not edit -*- lexical-binding: t; -*-

(require 'project)
(require 'vterm)

(defvar project-@PROVIDER@/prompt-regex)
(defvar project-@PROVIDER@/invocation)

(defmacro project-@PROVIDER@/ensure-ready (&rest body)
  (declare (indent 0))
  `(with-current-buffer (or (project-@PROVIDER@/get-buffer :no-solicit t)
			    (error "project-@PROVIDER@/get-buffer failed"))
     (when (project-@PROVIDER@//wait-for project-@PROVIDER@/prompt-regex)
       (when vterm-copy-mode
	 (vterm-copy-mode-done))
       ,@body)))

(cl-defun project-@PROVIDER@/get-buffer (&key no-solicit)
  "Get or create @PROVIDER_TITLE@ buffer for current project.

Use NO-SOLICIT if wanting to avoid pre-startup questions (as one
would if cold-starting from an in-band query)."
  (when-let ((proj (if (fboundp 'project-most-recent-project)
                       (funcall 'project-most-recent-project)
                     (project-current)))
	     (default-directory (project-root proj))
	     (vterm-shell
	      (format "/bin/sh -c '%s'"
		      (concat (when no-solicit
				"DISABLE_TELEMETRY=1 DISABLE_AUTOUPDATER=1 ")
			      project-@PROVIDER@/invocation)))
	     (buf (get-buffer-create (format "*@PROVIDER@-%s*" (project-name proj)))))
    (prog1 buf
      (with-current-buffer buf
	(when (or (not vterm--term)
		  (not (process-live-p vterm--process)))
	  (vterm-mode))))))

;;;###autoload (require 'project-@PROVIDER@)
(cl-defun project-@PROVIDER@ (&key no-solicit)
  "Returns @PROVIDER_TITLE@ buffer for current project.

Use NO-SOLICIT if wanting to avoid pre-startup questions (as one
would if cold-starting from an in-band query)."
  (interactive)
  (when-let ((buf (project-@PROVIDER@/get-buffer :no-solicit no-solicit)))
    (pop-to-buffer buf '((display-buffer-use-some-window) . ((some-window . mru))))))

(defvar project-@PROVIDER@/prompt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'project-@PROVIDER@/prompt-send)
    (define-key map (kbd "C-c C-k") 'project-@PROVIDER@/prompt-cancel)
    map)
  "Keymap for `project-@PROVIDER@/prompt-mode'.")

(define-derived-mode project-@PROVIDER@/prompt-mode text-mode "@PROVIDER_TITLE@-Prompt"
  "Major mode for entering prompts to send to @PROVIDER_TITLE@.
\\{project-@PROVIDER@/prompt-mode-map}"
  (setq header-line-format
        "Enter prompt for @PROVIDER_TITLE@. C-c C-c to send, C-c C-k to cancel"))

(defun project-@PROVIDER@/prompt-cancel ()
  "Call it off."
  (interactive)
  (quit-window t))

(cl-defun project-@PROVIDER@//wait-for (regex &key
					(from (point-min))
					(timeout 10)
					(absence))
  "Return t on success."
  (cl-loop with success
	   repeat (truncate (/ timeout .05))
	   until (setq success (save-excursion
				 (goto-char from)
				 (funcall (if absence #'not #'identity)
					  (re-search-forward regex nil t))))
	   do (accept-process-output vterm--process 0.05 nil t)
	   finally return success))

(defun project-@PROVIDER@//cursor-pos ()
  (save-excursion
    (goto-char (point-max))
    (re-search-backward project-@PROVIDER@/prompt-regex nil t)
    (when-let ((x (text-property-search-forward
		   'font-lock-face t
		   (lambda (value prop)
		     "What asshole wrote and documented t-p-s-f."
		     (and (listp prop)
			  (eq value (plist-get prop :inverse-video)))))))
      (prop-match-beginning x))))

(defun project-@PROVIDER@/say (what)
  "Say WHAT."
  (project-@PROVIDER@/ensure-ready
   ;; a simple vterm-send-string followed by vterm-send-key of
   ;; <return> results in newline-terminated string and no
   ;; submission.
   (let ((from (save-excursion
		 (goto-char (point-max))
		 (re-search-backward project-@PROVIDER@/prompt-regex nil t)))
	 (last-line (cl-loop for line in (reverse (split-string what "\n"))
			     when (not (string-empty-p (string-trim line)))
			     return line))
	 (mash (lambda (f)
		 ;; PREVIOUS should be initialized to cursor-pos.
		 ;; But need to go round at least twice else no-worky.
		 (cl-loop with previous
			  do (funcall f)
			  do (accept-process-output vterm--process 0.05 nil t)
			  for current = (project-@PROVIDER@//cursor-pos)
			  until (equal previous current)
			  do (setq previous current)))))
     (let ((inhibit-read-only t))
       ;; best effort to clear any residual crap before sending
       (funcall mash (apply-partially #'vterm-send-key "<down>"))
       (funcall mash (apply-partially #'vterm-send-key "e" nil nil :ctrl))
       (funcall mash (apply-partially #'vterm-send-key "<backspace>"))
       (vterm-send-string what))
     (project-@PROVIDER@//wait-for (replace-regexp-in-string
				    "[[:space:]]+" "\\\\s-+"
				    (regexp-quote last-line))
				   :from from))
   (let ((inhibit-read-only t))
     (vterm-send-key "<return>"))
   (setq this-command 'vterm-send-key)	;for vterm--filter
   ))

(defun project-@PROVIDER@/prompt-send ()
  "Send prompt buffer contents to @PROVIDER_TITLE@ and close prompt buffer."
  (interactive)
  (when-let ((source-buf (window-buffer (get-mru-window nil nil t))))
    (with-current-buffer source-buf
      (deactivate-mark)))
  (when-let ((prompt (buffer-substring-no-properties (point-min) (point-max)))
	     (not-empty-p (not (string-empty-p (string-trim prompt)))))
    (quit-window t)
    (let ((buf (project-@PROVIDER@ :no-solicit t)))
      (cl-assert (eq buf (current-buffer)))
      (project-@PROVIDER@/say prompt))))

;;;###autoload (require 'project-@PROVIDER@)
(defun project-@PROVIDER@/prompt ()
  "Open a prompt buffer to send queries to @PROVIDER_TITLE@."
  (interactive)
  (when (> (length (window-list)) 2)
    (delete-other-windows))
  (let ((file-ref (project-@PROVIDER@/file-reference))
	(buf (get-buffer-create "*@PROVIDER@-prompt*")))
    (with-current-buffer buf
      (project-@PROVIDER@/prompt-mode)
      (erase-buffer)
      (when file-ref (insert (format "\"%s\"" file-ref) " ")))
    (pop-to-buffer buf '((display-buffer-at-bottom)
			 (window-height . 5)))))

(defun project-@PROVIDER@/file-reference ()
  "Construct @PROVIDER_TITLE@ file reference from current position."
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

(define-minor-mode project-@PROVIDER@-mode
  "Minor mode for @PROVIDER_TITLE@ integration."
  :group 'project-@PROVIDER@
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c '") #'project-@PROVIDER@/prompt)
            map))

;;;###autoload (require 'project-@PROVIDER@)
(define-globalized-minor-mode global-project-@PROVIDER@-mode
  project-@PROVIDER@-mode (lambda ()
			(when (derived-mode-p 'prog-mode)
			  (project-@PROVIDER@-mode 1)))
  :group 'project-@PROVIDER@)

(provide 'project-@PROVIDER@-generated)
;;; project-@PROVIDER@-generated.el ends here
