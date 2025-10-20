;;; project-gemini.el --- A project.el plugin -*- lexical-binding: t; -*-

;; Copyright (C) 2025 dickmao
;;
;; Author: dickmao
;; Version: 0.0.1
;; URL: https://github.com/dickmao/project-gemini
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

(defconst project-gemini/prompt-regex "│\\s-*>\\s-+" "Gemini CLI prompt.")

(defgroup project-gemini nil
  "Integration with Gemini Code CLI."
  :group 'tools
  :prefix "project-gemini/")

(defcustom project-gemini/invocation "npx @google/gemini-cli@latest"
  "Command line shell invocation."
  :group 'project-gemini
  :type 'string)

(require 'project-gemini-generated)

(provide 'project-gemini)
;;; project-gemini.el ends here
