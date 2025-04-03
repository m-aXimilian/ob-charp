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

(org-test-for-executable "dotnet")

(defun find-dotnet-version ()
  "Get a list of dotnet major versions from a list of dotnet sdks."
  (mapcar #'(lambda (n)
              (let ((fr (string-match "^[0-9.]+\\." n))
                    (to (string-match "\\." n)))
                (string-to-number (substring n fr to))))
          (split-string (shell-command-to-string "dotnet --list-sdks") "\n")))

(defvar dotnet-version (format "net%s.0" (apply #'max (find-dotnet-version))))

(provide 'test-ob-csharp)
;;; test-ob-csharp.el ends here
