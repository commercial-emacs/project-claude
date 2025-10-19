;;; test-project-gemini.el --- Tests for project-gemini -*- lexical-binding: t; -*-

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
;; Tests for project-gemini.

;;; Code:

(require 'ert)
(require 'project-gemini)

(ert-deftest basic ()
  (let* ((project-gemini/invocation "echo foo && sleep 20")
	 (buf (project-gemini :no-solicit t)))
    (should (eq buf (current-buffer)))
    (should (project-gemini//wait-for (regexp-quote "foo")))))

(provide 'test-project-gemini)
;;; test-project-gemini.el ends here
