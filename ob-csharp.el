;;; ob-csharp.el --- org-babel functions for csharp evaluation -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025 Free Software Foundation, Inc.

;; Author:     Maximilian Kueffner
;; Maintainer: Maximilian Kueffner <poverobuosodonati@gmail.com>
;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Requirements:

;; Some .NET runtime environment should be installed.
;; The `dotnet' command should be available to the system's environment
;; (PATH discoverable for example).

;;; Code:
(require 'ob)

;; file extension for C#
(add-to-list 'org-babel-tangle-lang-exts '("csharp" . "cs"))

;; default header arguments for C#
(defvar org-babel-default-header-args:csharp
  '((main . ((yes no)))
    (namespace . :any)
    (nugetconfig . :any)
    (framework . :any)
    (project . :any)
    (class . ((no nil :any)))
    (references . :any)
    (usings . :any))
  "Csharp specific header arguments.")

(defcustom org-babel-csharp-compiler "dotnet"
  "The program to call for compiling a csharp project.")

(defcustom org-babel-csharp-default-target-framework "net7.0"
  "The desired target framework to use.")

(defvar org-babel-csharp-additional-project-flags nil
  "Will be passed in the 'PropertyGroup' defining the project.

This is taken as-is. It should be a string in XML-format.")

(defcustom org-babel-csharp-generate-compile-command
  '(lambda (base-dir bin-dir)
     (format "%s build --output %S %S"
             org-babel-csharp-compiler bin-dir base-dir))
  "A function creating the compile command.
It must take two parameters intended for the target binary directory and
the base directory where the csproj-file resides in."
  :type 'function)

(defcustom org-babel-csharp-generate-restore-command
  '(lambda (project-file)
     (format "%s restore %S" org-babel-csharp-compiler project-file))
  "A function creating a project restore command.
It must take one parameter defining the project to perform a restore on."
  :type 'function)

(defun org-babel--csharp-generate-project-file (refs type namespace framework)
  "Construct a csproj file from a list of REFS based on the project TYPE with the root NAMESPACE."
  (concat "<Project Sdk=\"Microsoft.NET.Sdk\">\n\n  "
          (when refs
            (org-babel--csharp-format-refs refs))
          "\n\n  <PropertyGroup>"
          (unless (eq type 'class)
            (format "\n    <OutputType>Exe</OutputType>\n    <RootNamespace>%s</RootNamespace>" namespace))
          (format "\n    <TargetFramework>%s</TargetFramework>" framework)
          "\n    <ImplicitUsings>enable</ImplicitUsings>"
          "\n    <Nullable>enable</Nullable>"
          (when org-babel-csharp-additional-project-flags
            (format "\n    %s" org-babel-csharp-additional-project-flags))
          "\n  </PropertyGroup>"
          "\n</Project>"))

(defun org-babel--csharp-preprocess-params (params)
  "Make sure PARAMS contains a cons-cell for  both `:project' and `:namespace'."
  (unless (assoc :project params)
    (push `(:project . ,(symbol-name (gensym))) params))
  (unless (assoc :namespace params)
    (push `(:namespace . ,(symbol-name (gensym))) params))
  params)

(defun org-babel--csharp-format-usings (usings)
  (let ((usinglist))
    (setf usinglist (mapconcat #'(lambda (u) (format "using %s;" u)) usings "\n"))
    usinglist))

(defun org-babel-expand-body:csharp (body params ;; processed-params
                                          )
  (let* ((main-p (not (string= (cdr (assq :main params)) "no")))
         (class (pcase (alist-get :class params)
                  ("no" nil) ;; class explicitly
                  (`nil "Program")
                  (t (alist-get :class params))))
         (params (org-babel--csharp-preprocess-params params))
         (namespace (alist-get :namespace params))
         (usings (alist-get :usings params))
         (vars (org-babel--get-vars params)))
    (with-temp-buffer
      (insert "namespace " namespace ";\n")
      (when usings
        (insert (format "\n%s\n" (org-babel--csharp-format-usings usings))))
      (when class
        (insert "\nclass " class "\n{\n"))
      (when main-p
        (insert "static void Main(string[] args)\n{\n"))
      (let ((start (point)))
        (insert (mapconcat 'org-babel-csharp-var-to-csharp vars "\n") "\n")
        (insert body))
      (when main-p
        (insert "\n}"))
      (when class
        (insert "\n}"))
      (buffer-string))))

(defun org-babel--csharp-format-refs (refs)
  (let ((projectref)
        (assemblyref)
        (systemref))
    (dolist (ref refs)
      (let* ((version (pcase (type-of ref)
                        ('cons (cdr ref))
                        (_ nil)))
             (ref-string (pcase (type-of ref)
                           ('cons (car ref))
                           (_ ref)))
             (full-ref (if version
                           (file-truename (car ref))
                         (file-truename ref)))
             (use-fill-ref-p (file-exists-p full-ref)))
        ;; (unless (file-exists-p full-ref)
        ;;   (error (format "Reference %S not found" full-ref)))
        (cond
         ((string-search ".csproj" full-ref)
          (setf projectref
                (concat projectref
                        (format "\n    <ProjectReference Include=\"%s\" />"
                                full-ref))))
         ((string-search ".dll" full-ref)
          (setf assemblyref
                (concat assemblyref
                        (format "\n    <Reference Include=%S>\n      <HintPath>%s</HintPath>\n    </Reference>"
                                (file-name-base full-ref) full-ref))))
         (t (setf systemref
                  (concat systemref
                          (format "\n    <PackageReference Include=%s />"
                                  (if version
                                      (format "%S Version=%S" ref-string version)
                                    (format "%S" ref-string)))))))))
    (format "%s\n\n  %s\n\n  %s"
            (if projectref
                (format "<ItemGroup>%s\n  </ItemGroup>" projectref)
              "")
            (if assemblyref
                (format "<ItemGroup>%s\n  </ItemGroup>" assemblyref)
              "")
            (if systemref
                (format "<ItemGroup>%s\n  </ItemGroup>" systemref)
              ""))))

(defun ensure-directories-exist ()
  "Ensure the current working directory exists.

Within the context of a possibly non-existing working directory, this will
recursively create the missing directories of the current working directory's path."
  (let ((full-name (file-truename ".")))
    (unless (file-exists-p full-name)
      (make-directory full-name t))
    full-name))

(defun org-babel-execute:csharp (body params)
  "Execute a block of Csharp code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing Csharp source code block")
  (let* ((params (org-babel--csharp-preprocess-params params))
         (vars (org-babel--get-vars params))
         (result-params (assq :result-params params))
         (result-type (assq :result-type params))
         (full-body (org-babel-expand-body:csharp body params))
         (project-name (alist-get :project params))
         (namespace (alist-get :namespace params))
         (dir-param (alist-get :dir params))
         (base-dir (file-name-concat (if dir-param
                                         (ensure-directories-exist)
                                       org-babel-temporary-directory)
                                     project-name))
         (bin-dir (file-name-concat base-dir "bin"))
         (framework (or (alist-get :framework params) org-babel-csharp-default-target-framework))
         (program-file (file-name-concat base-dir "Program.cs"))
         (project-file (file-name-concat base-dir (concat project-name ".csproj")))
         (nuget-file (alist-get :nugetconfig params))
         (project-type (if (and (equal "no" (alist-get :main params))
                                (equal "no" (alist-get :class params)))
                           'class
                         'executable))
         (restore-cmd (funcall org-babel-csharp-generate-restore-command project-file))
         (compile-cmd (funcall org-babel-csharp-generate-compile-command
                               (file-truename base-dir)
                               (file-truename bin-dir)))
         (run-cmd (format "%S" (file-truename (file-name-concat bin-dir project-name)))))
    (unless (file-exists-p base-dir)
      (make-directory base-dir))
    (with-temp-file program-file
      (insert full-body))
    (with-temp-file project-file
      (insert
       (let ((refs (alist-get :references params)))
         (org-babel--csharp-generate-project-file refs project-type namespace framework))))
    (when (and nuget-file (file-exists-p (file-truename nuget-file)))
      (copy-file nuget-file (file-name-concat base-dir (file-name-nondirectory (file-truename nuget-file)))))
    ;; nuget restore
    (message restore-cmd)
    (org-babel-eval restore-cmd "")
    (message compile-cmd)
    (org-babel-eval compile-cmd "")
    (let ((results (unless (eq project-type 'class)
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
	  (cdr (assq :rowname-names params)) (cdr (assq :rownames params))))))))

(defun org-babel-csharp-var-to-csharp (var)
  "Convert an elisp var into a string of csharp source code
specifying a var of the same value."
  (format "var %s = %S;" (car var) (cdr var)))

(provide 'ob-csharp)
;;; ob-csharp.el ends here
