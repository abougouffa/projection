;;; projection-multi-vscode-tasks.el --- Projection integration for `compile-multi' and the VScode tasks.json. -*- lexical-binding: t; -*-

;; Copyright (C) 2023, 2026  Mohsin Kaleem

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library exposes a target generation function for `compile-multi' which
;; sources the list of available targets from a VSCode task.json file. See
;; https://code.visualstudio.com/docs/editor/tasks#_custom-tasks

;;; Code:

(require 'json)
(require 'projection-multi)
(require 'projection-types)

(defgroup projection-multi-vscode-tasks nil
  "Helpers for `compile-multi' and VSCode task projects."
  :group 'projection-multi)

(defcustom projection-multi-vscode-tasks-cache-tasks 'auto
  "When true cache the VSCode task targets of each project."
  :type '(choice
          (const :tag "Cache targets and invalidate cache automatically" auto)
          (boolean :tag "Always/Never cache targets")))

(defcustom projection-multi-vscode-workspace-dir nil
  "Set the value for VSCode's ${workspaceFolder}.
This is intented to be set in .dir-locals if needed.
Defaults to the `project-root' if not specified."
  :type '(choice
          (const :tag "Unset, use project-root" nil)
          (directory :tag "A directory for VSCode's workspaceFolder variable")))

(defun projection-multi-vscode-tasks--contents ()
  "Read VSCode tasks file respecting project-cache."
  (projection--cache-get-with-predicate
   (projection--current-project 'no-error)
   'projection-multi-vscode-tasks
   (cond
    ((eq projection-multi-vscode-tasks-cache-tasks 'auto)
     (projection--cache-modtime-predicate ".vscode/tasks.json"))
    (t projection-multi-vscode-tasks-cache-tasks))
   #'projection-multi-vscode-tasks--contents2))

(projection--declare-cache-var
  'projection-multi-vscode-tasks
  :title "Multi VSCode tasks"
  :category "VSCode"
  :description "VSCode tasks associated with this project"
  :hide t)

(defun projection-multi-vscode-tasks--contents2 ()
  "Read VSCode tasks file."
  (projection--log :debug "Reading VSCode tasks.json")
  (condition-case err
      (let ((json-array-type 'list))
        (json-read-file ".vscode/tasks.json"))
    ((file-missing json-readtable-error)
     (projection--log :error "Failed to read VSCode tasks.json: %S." (cdr err)))))

(defun projection-multi-vscode-tasks--workspace-dir (&optional dir)
  (or projection-multi-vscode-workspace-dir
      (when-let* ((prj (project-current nil dir)))
        (project-root prj))))

;; Specs at https://code.visualstudio.com/docs/reference/variables-reference
(defun projection-multi-vscode-tasks--predefined-var (var)
  "Return the value for VSCode's predefined variable VAR."
  (pcase var
    ("userHome" (expand-file-name "~"))
    ("workspaceFolder" (projection-multi-vscode-tasks--workspace-dir))
    ("workspaceFolderBasename" (file-name-nondirectory (directory-file-name (projection-multi-vscode-tasks--workspace-dir))))
    ("file" (buffer-file-name))
    ("fileBasename" (file-name-nondirectory (buffer-file-name)))
    ("fileBasenameNoExtension" (file-name-base (buffer-file-name)))
    ("fileExtname" (file-name-extension (buffer-file-name) t))
    ("fileDirname" (directory-file-name (file-name-directory (buffer-file-name))))
    ("fileDirnameBasename" (file-name-nondirectory (directory-file-name (file-name-directory (buffer-file-name)))))
    ("lineNumber" (int-to-string (line-number-at-pos)))
    ("columnNumber" (int-to-string (- (point) (line-beginning-position))))
    ("selectedText" (when (region-active-p) (buffer-substring-no-properties (region-beginning) (region-end))))
    ("execPath" (directory-file-name (file-name-directory (car command-line-args))))
    ((or "pathSeparator" "/") (if (memq system-type '(windows-nt ms-dos)) "\\" "/"))
    ("fileWorkspaceFolder"
     (let ((default-directory (file-name-directory (buffer-file-name))))
       (projection-multi-vscode-tasks--workspace-dir)))
    ("relativeFile" (file-relative-name (buffer-file-name) (projection-multi-vscode-tasks--workspace-dir)))
    ("relativeFileDirname" (directory-file-name (file-name-directory (file-relative-name (buffer-file-name) (projection-multi-vscode-tasks--workspace-dir)))))
    ("cwd") ; TODO: Not clear for now!
    ("defaultBuildTask"))) ; TODO: Not clear for now!

(defun projection-multi-vscode-tasks--input-var (var)
  "Read variable VAR from the user."
  (let ((result))
    (dolist (input (alist-get 'inputs (projection-multi-vscode-tasks--contents)))
      (let-alist input
        (when (equal .id var)
          (let ((prompt (concat (or .description (concat "Choose an option" (when .id (concat "for " .id)))) " ")))
            (pcase .type
              ("pickString"
               (setq result (completing-read prompt (mapcar (lambda (opt)
                                                              (if (json-alist-p opt)
                                                                  (alist-get 'value opt)
                                                                opt))
                                                            .options)
                                             nil nil .default)))
              ("promptString"
               (setq result (if (eq .password t)
                                (read-passwd prompt nil .default)
                              (read-string prompt .default))))
              ("command" (projection--log :warning "Unsupported input of type \"commands\"")))))))
    (or result (user-error "Undefined VSCode's variable %s in tasks.json" var))))

(defun projection-multi-vscode-tasks--var (name)
  "Get the value of variable NAME."
  (cond ((string-prefix-p "env:" name)
         (getenv (string-remove-prefix "env:" name)))
        ((string-prefix-p "input:" name)
         (projection-multi-vscode-tasks--input-var (string-remove-prefix "input:" name)))
        ((string-prefix-p "config:" name)
         (projection--log :warning "Variable of type config are not supported"))
        (t (projection-multi-vscode-tasks--predefined-var name))))

(defun projection-multi-vscode-tasks--substitute-vars (str)
  "Substitute variables in STR."
  (let ((start -1) var-names)
    (while (setq start (string-match "\\${\\([^}]*\\)}" str (1+ start)))
      (push (match-string 1 str) var-names))
    (dolist (var-name (cl-remove-duplicates (reverse var-names)))
      (setq str (string-replace (format "${%s}" var-name) (projection-multi-vscode-tasks--var var-name) str)))
    str))



;;;###autoload
(defun projection-multi-compile-vscode-targets (&optional project-type)
  "`compile-multi' target generator function for VSCode task projects.
When set the generated targets will be prefixed with PROJECT-TYPE."
  (setq project-type (or project-type "vscode"))

  (let ((result))
    (dolist (task (alist-get 'tasks (projection-multi-vscode-tasks--contents)))
      (let-alist task
        (setq .type (intern (or .type "shell")))
        (when (consp .group)
          (setq .group (alist-get 'kind .group)))

        (when (and (cl-member .type '(process shell))
                   .command)
          (push (cons (concat project-type ":"
                              (when .group
                                (concat .group ":"))
                              (or .label .command))
                      (lambda ()
                        (concat
                         (projection--join-shell-command
                          (projection--env-shell-command-prefix
                           (cl-loop for (key . value) in (alist-get 'env .options)
                                    collect (cons (symbol-name key) (projection-multi-vscode-tasks--substitute-vars value)))
                           (alist-get 'cwd .options)))
                         (let ((cmd (projection-multi-vscode-tasks--substitute-vars .command)))
                           (if (eq .type 'shell) cmd (shell-quote-argument cmd)))
                         (projection--join-shell-command
                          (cl-loop for arg in .args
                                   when (consp arg)
                                   collect (projection-multi-vscode-tasks--substitute-vars (alist-get 'value arg))
                                   else
                                   collect (projection-multi-vscode-tasks--substitute-vars arg))))))
                result))))
    (nreverse result)))

;;;###autoload
(defun projection-multi-compile-vscode-tasks ()
  "`compile-multi' wrapper for only VSCode task targets."
  (interactive)
  (projection-multi-compile--run
   (projection--current-project 'no-error)
   `((t ,#'projection-multi-compile-vscode-targets))))

;;;###autoload
(with-eval-after-load 'projection-types
  (projection-type-append-compile-multi-targets projection-project-type-vscode-tasks
    #'projection-multi-compile-vscode-targets))

(provide 'projection-multi-vscode-tasks)
;;; projection-multi-vscode-tasks.el ends here
