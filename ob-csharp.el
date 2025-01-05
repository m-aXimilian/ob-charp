;;; ob-csharp.el --- org-babel functions for csharp evaluation -*- lexical-binding: t -*-

;; Copyright (C) Maximilian Kueffner

;; Author: Maximilian Kueffner
;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Requirements:

;; Some .NET runtime environment should be installed.
;; The `dotnet' command should be available to the system's environment
;; (PATH discoverable for example).

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
;; possibly require modes required for your language

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("csharp" . "cs"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:csharp
  '((main . :any)
    (namespace . :any)
    (project . :any)
    (project-type . :any)
    (namespace)
    (class :any)
    (references :any)
    (usings :any))
  "Csharp specific header arguments.")

(defcustom org-babel-csharp-compiler "dotnet"
  "The program to call for compiling a csharp project.")

(defun org-babel--parse-project-file (refs type namespace)
  "Construct a csproj file from a list of REFS based on the project TYPE with the root NAMESPACE."
  (concat "<Project Sdk=\"Microsoft.NET.Sdk\">\n\n  "
          (when refs
            (org-babel--csharp-parse-refs refs))
          "\n\n  <PropertyGroup>"
          (unless (equal type "class")
            (format "\n    <OutputType>Exe</OutputType>
    <RootNamespace>%s</RootNamespace>" namespace))
    "\n    <TargetFramework>net7.0</TargetFramework>
    <ImplicitUsings>enable</ImplicitUsings>
    <Nullable>enable</Nullable>
  </PropertyGroup>
\n</Project>"))

(defun org-babel--csharp-preprocess-params (params)
  "Make sure PARAMS contains a cons-cell for  both `:project' and `:namespace'."
  (unless (assoc :project params)
    (push `(:project . ,(symbol-name (gensym))) params))
  (unless (assoc :namespace params)
    (push `(:namespace . ,(symbol-name (gensym))) params))
  params)

(defun org-babel--csharp-parse-usings (usings)
  (let ((usinglist))
    (dolist (using usings)
      (setf usinglist (concat usinglist (format "using %s;\n" using))))
    usinglist))

(defun org-babel-expand-body:csharp (body params ;; processed-params
                                          )
  (let* ((main-p (not (string= (cdr (assq :main params)) "no")))
         (class-tmp (alist-get :class params))
         (class (pcase class-tmp
                  ("no" nil) ;; class explicitly
                  (`nil "Program")
                  (t class-tmp)))
         (params (org-babel--csharp-preprocess-params params))
         (namespace (alist-get :namespace params))
         (usings (alist-get :usings params)))
    (with-temp-buffer
      (insert "namespace " namespace ";\n")
      (when usings
        (insert (format "\n%s\n" (org-babel--csharp-parse-usings usings))))
      (when class
        (insert "\nclass " class "\n{\n"))
      (when main-p
        (insert "static void Main(string[] args)\n{\n"))
      (let ((start (point)))
        (insert body))
      (when main-p
        (insert "\n}"))
      (when class
        (insert "\n}"))
      (buffer-string))))

(defun org-babel--csharp-parse-refs (refs)
  (let ((itemgroup "<ItemGroup>"))
    (dolist (ref refs)
      (let ((full-ref (file-truename ref)))
        (unless (file-exists-p full-ref)
          (error (format "Reference %S not found" full-ref)))
      (setf itemgroup (concat itemgroup (format "\n    <ProjectReference Include=\"%s\" />" full-ref)))))
    (concat itemgroup "\n  </ItemGroup>")))

(defun org-babel-execute:csharp (body params)
  "Execute a block of Csharp code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing Csharp source code block")
  (org-babel--csharp-preprocess-params params)
  (let* ((processed-params (org-babel-process-params params))
         (params (org-babel--csharp-preprocess-params params))
         (vars (org-babel--get-vars processed-params))
         (result-params (assq :result-params processed-params))
         (result-type (assq :result-type processed-params))
         (full-body (org-babel-expand-body:csharp
                     body params ;; processed-params
                     ))
         (project-name (alist-get :project params))
         (namespace (alist-get :namespace params))
         (base-dir (file-name-concat (file-truename ".") project-name))
         (bin-dir (file-name-concat base-dir "bin"))
         (program-file (file-name-concat base-dir "Program.cs"))
         (project-file (file-name-concat base-dir (concat project-name ".csproj")))
         (project-type (alist-get :project-type params))
         (compile-cmd (concat org-babel-csharp-compiler " " "build" " " "--output" " " bin-dir " " (file-truename base-dir)))
         (run-cmd (file-truename (file-name-concat bin-dir project-name))))
    (unless (file-exists-p base-dir)
      (make-directory base-dir))
    (with-temp-file program-file
      (insert full-body))
    (with-temp-file project-file
      (insert
       (let ((refs (alist-get :references params)))
         (org-babel--parse-project-file refs project-type namespace))))
    (message compile-cmd)
    (org-babel-eval compile-cmd "")
    (let ((results (unless (string= project-type "class")
                     (org-babel-eval run-cmd ""))))
      (when results
        (setq results (org-remove-indentation results))
        ;; results
        (org-babel-reassemble-table
	 (org-babel-result-cond (cdr (assq :result-params params))
	   results
	   (let ((tmp-file (org-babel-temp-file "c-")))
	     (with-temp-file tmp-file (insert results))
	     (org-babel-import-elisp-from-file tmp-file)))
	 (org-babel-pick-name
	  (cdr (assq :colname-names params)) (cdr (assq :colnames params)))
	 (org-babel-pick-name
	  (cdr (assq :rowname-names params)) (cdr (assq :rownames params))))
        ))))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:csharp (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  )

(defun org-babel-csharp-var-to-csharp (var)
  "Convert an elisp var into a string of csharp source code
specifying a var of the same value."
  (format "%S" var))

(defun org-babel-csharp-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  )

(defun org-babel-csharp-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (unless (string= session "none")
    ))

(provide 'ob-csharp)
;;; ob-csharp.el ends here
