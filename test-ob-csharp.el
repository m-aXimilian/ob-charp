;;; test-ob-csharp.el --- Tests for ob-csharp        -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Maximilian Kueffner

;; Author: Maximilian Kueffner <poverobuosodonati@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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

;;

;;; Code:
(require 'ob-core)

(unless (featurep 'ob-csharp)
  (signal 'missing-test-dependency '("Support for C# code blocks")))

(ert-deftest test-ob-csharp/lackmus ()
  (should (= 1 1)))

(org-test-for-executable org-babel-csharp-compiler)

(defun find-dotnet-version ()
  "Get a list of dotnet major versions from a list of dotnet sdks."
  (mapcar #'(lambda (n)
              (let ((fr (string-match "^[0-9.]+\\." n))
                    (to (string-match "\\." n)))
                (string-to-number (substring n fr to))))
          (split-string (shell-command-to-string (format "%s --list-sdks" org-babel-csharp-compiler)) "\n")))

(defvar system-dotnet-version (format "net%s.0" (apply #'max (find-dotnet-version)))
  "The most recent dotnet version of the host system.")

(defun try-set-dotnet-version (version)
  ""
  (when version
    (setq org-babel-csharp-default-target-framework version)))

(defmacro with-newest-dotnet (&rest body)
  "Set the most recent dotnet version in context."
  `(progn
     (try-set-dotnet-version system-dotnet-version)
     ,@body))

(ert-deftest test-ob-csharp/int-from-var ()
  "Test of an integer variable."
  (with-newest-dotnet
   (org-test-at-id "FC200076-9BB6-4070-B078-BB053262A8CD"
     (org-babel-next-src-block 1)
     (should (= 42 (org-babel-execute-src-block))))))

(ert-deftest test-ob-csharp/float-from-var ()
  "Test of a float variable."
  (with-newest-dotnet
   (org-test-at-id "FC200076-9BB6-4070-B078-BB053262A8CD"
     (org-babel-next-src-block 2)
     (should (= 3.14 (org-babel-execute-src-block))))))

(ert-deftest test-ob-csharp/string-from-var ()
  "Test of a string variable."
  (with-newest-dotnet
   (org-test-at-id "FC200076-9BB6-4070-B078-BB053262A8CD"
     (org-babel-next-src-block 3)
     (should (string= "pi" (org-babel-execute-src-block))))))

(ert-deftest test-ob-csharp/outputs-list ()
  "Test list output."
  (with-newest-dotnet
   (org-test-at-id "FC200076-9BB6-4070-B078-BB053262A8CD"
     (org-babel-next-src-block 4)
     (should (equal "Item 1\nItem 2\nItem 3\nItem 4\n" (org-babel-execute-src-block))))))

(ert-deftest test-ob-csharp/commandline-input ()
  "Test command line input."
  (with-newest-dotnet
   (org-test-at-id "FC200076-9BB6-4070-B078-BB053262A8CD"
     (org-babel-next-src-block 5)
     (should (= 42 (org-babel-execute-src-block))))))

(ert-deftest test-ob-csharp/custom-class-and-main ()
  "Test custom class with custom main function."
  (with-newest-dotnet
   (org-test-at-id "FC200076-9BB6-4070-B078-BB053262A8CD"
     (org-babel-next-src-block 6)
     (should (= 123 (org-babel-execute-src-block))))))

(ert-deftest test-ob-csharp/tabular-format-output ()
  "Test for tabular output format."
  (with-newest-dotnet
   (org-test-at-id "FC200076-9BB6-4070-B078-BB053262A8CD"
     (org-babel-next-src-block 7)
     (should (equal '(("In" "questo" "mondo" "una" "cosa")
                      ("si" "perde" "una" "si" "trova"))
                    (org-babel-execute-src-block))))))

(ert-deftest test-ob-csharp/nuget-reference ()
  "Test with nuget reference."
  (with-newest-dotnet
   (org-test-at-id "FC200076-9BB6-4070-B078-BB053262A8CD"
     (org-babel-next-src-block 8)
     (should (string= "{\n  \"TheInt\": 12,\n  \"TheString\": \"ok\"\n}\n"
                    (org-babel-execute-src-block))))))

(provide 'test-ob-csharp)
;;; test-ob-csharp.el ends here
