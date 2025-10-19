;;; This is a template file for generating project-@PROVIDER@-generated.el

(require 'project)
(require 'vterm)

;;;###autoload
(cl-defun project-@PROVIDER@ (&key no-solicit)
  "Returns @PROVIDER_TITLE@ buffer for current project.

Use NO-SOLICIT if wanting to avoid pre-startup questions (as one
would if cold-starting from an in-band query)."
  (interactive)
  (when-let ((proj (if (fboundp 'project-most-recent-project)
                       (funcall 'project-most-recent-project)
                     (project-current)))
	     (default-directory (project-root proj))
	     (buf (get-buffer-create (format "*@PROVIDER@-%s*" (project-name proj)))))
    (if (with-current-buffer buf (and vterm--term (process-live-p vterm--process)))
	(pop-to-buffer buf '((display-buffer-use-some-window) . ((some-window . mru))))
      (let ((vterm-shell
	     (format "/bin/sh -c '%s'"
		     (concat (when no-solicit
			       "DISABLE_TELEMETRY=1 DISABLE_AUTOUPDATER=1 ")
			     project-@PROVIDER@/invocation)))
	    (vterm-buffer-name (buffer-name buf)))
	(vterm-other-window)))))

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

(defun project-@PROVIDER@//wait-for (regex &optional from)
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

(defun project-@PROVIDER@/cursor-pos ()
  (save-excursion
    (goto-char (point-max))
    (re-search-backward project-@PROVIDER@/prompt-regex nil t)
    (prop-match-beginning
     (text-property-search-forward
      'font-lock-face t
      (lambda (value prop)
	"What asshole wrote and documented t-p-s-f."
	(and (listp prop)
	     (eq value (plist-get prop :inverse-video))))))))

(defun project-@PROVIDER@/issue-this (what)
  "You can send commands willy-nilly to bash.
But something about @PROVIDER_TITLE@'s input processing interprets a too-soon
RET as M-RET."
  (when (project-@PROVIDER@//wait-for project-@PROVIDER@/prompt-regex)
    (when vterm-copy-mode
      (vterm-copy-mode-done))
    ;;(project-@PROVIDER@/clear-input)
    (let ((start (window-start)))
      (let ((inhibit-read-only t))
	(vterm-send-string what))
      (when (project-@PROVIDER@//wait-for (regexp-quote what) start)
	(let ((inhibit-read-only t))
	  (vterm-send-key "<return>"))
	(setq this-command 'vterm-send-key))) ;for vterm--filter
    ))

(defun project-@PROVIDER@/prompt-send ()
  "Send prompt buffer contents to @PROVIDER_TITLE@ and close prompt buffer."
  (interactive)
  (when-let ((prompt (buffer-substring-no-properties (point-min) (point-max)))
	     (not-empty-p (not (string-empty-p (string-trim prompt)))))
    (quit-window t)
    (let ((buf (project-@PROVIDER@ :no-solicit t)))
      (cl-assert (eq buf (current-buffer)))
      (project-@PROVIDER@/issue-this prompt))))

;;;###autoload
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
      (when file-ref (insert file-ref " ")))
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

;;;###autoload
(define-globalized-minor-mode global-project-@PROVIDER@-mode
  project-@PROVIDER@-mode (lambda ()
			(when (derived-mode-p 'prog-mode)
			  (project-@PROVIDER@-mode 1)))
  :group 'project-@PROVIDER@)

(provide 'project-@PROVIDER@-generated)
;;; project-@PROVIDER@-generated.el ends here
