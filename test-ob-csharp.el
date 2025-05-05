;;; test-ob-csharp.el --- Tests for ob-csharp        -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Maximilian Kueffner

;; Author: Maximilian Kueffner <poverobuosodonati@gmail.com>

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

;;; Code:

(require 'ob-core)

(unless (featurep 'ob-csharp)
  (signal 'missing-test-dependency '("Support for C# code blocks")))

(ert-deftest test-ob-csharp/customized-compile-command-used ()
  "User specified compile command is used."
  (let* ((custom-fun (lambda (p b) (format "custom-compiler %s %s" p b)))
         (project "/tmp/placeholder/dummy.csproj")
         (binary "/tmp/placeholder/bin")
         (default-command (funcall org-babel-csharp-generate-compile-command project binary))
         (cmd-backup org-babel-csharp-generate-compile-command))
    (unwind-protect
        (progn
          (setq org-babel-csharp-generate-compile-command custom-fun)
          (should-not (string=
                       default-command
                       (funcall org-babel-csharp-generate-compile-command project binary)))
          (should (string= (funcall custom-fun project binary)
                           (funcall org-babel-csharp-generate-compile-command project binary))))
      (setq org-babel-csharp-generate-compile-command cmd-backup))))

(ert-deftest test-ob-csharp/customized-restore-command-used ()
  "User specified compile command is used."
  (let* ((custom-fun (lambda (p) (format "custom-restore %s" p)))
         (project "/tmp/placeholder/dummy.csproj")
         (default-command (funcall org-babel-csharp-generate-restore-command project))
         (cmd-backup org-babel-csharp-generate-restore-command))
    (unwind-protect
        (progn
          (setq org-babel-csharp-generate-restore-command custom-fun)
          (should-not (string=
                       default-command
                       (funcall org-babel-csharp-generate-restore-command project)))
          (should (string= (funcall custom-fun project)
                           (funcall org-babel-csharp-generate-restore-command project))))
      (setq org-babel-csharp-generate-restore-command cmd-backup))))

(ert-deftest test-ob-csharp/generate-project-file ()
  "Test intended parameterization of the project file generator."
  (should (eq 'string
              (type-of (org-babel--csharp-generate-project-file nil "net6.0"))))
  (should (eq 'string
              (type-of (org-babel--csharp-generate-project-file '("a-ref") "net6.0"))))
  (should (eq 'string
              (type-of (org-babel--csharp-generate-project-file '("a-ref" "b-ref") "net6.0"))))
  (should-error (org-babel--csharp-generate-project-file nil nil))
  (should-error (org-babel--csharp-generate-project-file nil nil))
  (should-error (org-babel--csharp-generate-project-file '(nil) "net6.0"))
  (should-error (org-babel--csharp-generate-project-file "a-ref" "net6.0")))

(ert-deftest test-ob-csharp/format-usings ()
  "Test intended parameterization of the C# using formatter."
  (should (string=
           "using namespaceA;\nusing namesaceB;"
           (org-babel--csharp-format-usings '("namespaceA" "namesaceB"))))
  (should (string=
           ""
           (org-babel--csharp-format-usings nil)))
  (should-error (org-babel--csharp-format-usings '("namespaceA" nil "namesaceB")))
  (should-error (org-babel--csharp-format-usings "singleUsing")))

(ert-deftest test-ob-csharp/custom-class-name-header-argument ()
  "The generated class name "
  (let ((cs-block (org-test-with-temp-text
                      "#+begin_src csharp :class \"MyAwesomeClass\"
  Console.WriteLine(\"ok\");
#+end_src"
                    (org-babel-expand-src-block))))
    (should     (string-search "class MyAwesomeClass" cs-block))
    (should-not (string-search "class Program" cs-block))))

;; requires dotnet compiler
(org-test-for-executable org-babel-csharp-compiler)

(defun test-ob-csharp--find-dotnet-version ()
  "Get a list of dotnet major versions from a list of dotnet sdks."
  (delete-if #'(lambda (v) (= 0 v))
             (delete-dups
              (mapcar #'(lambda (n)
                          (let ((fr (string-match "^[0-9.]+\\." n))
                                (to (string-match "\\." n)))
                            (string-to-number (substring n fr to))))
                      (split-string (shell-command-to-string (format "%s --list-sdks" org-babel-csharp-compiler)) "\n")))))

(defvar test-ob-csharp-system-dotnet-version (format "net%s.0" (apply #'max (test-ob-csharp--find-dotnet-version)))
  "The most recent dotnet version of the host system.")

(defmacro test-ob-csharp-with-newest-dotnet (&rest body)
  "Set the most recent dotnet version in context."
  (declare (indent 1))
  `(let ((reset-framework ,org-babel-csharp-default-target-framework))
     (unwind-protect
         (progn
           (setq org-babel-csharp-default-target-framework test-ob-csharp-system-dotnet-version)
           ,@body)
       (setq org-babel-csharp-default-target-framework reset-framework))))

(ert-deftest test-ob-csharp/int-from-var ()
  "Test of an integer variable."
  (test-ob-csharp-with-newest-dotnet
      (org-test-with-temp-text "#+begin_src csharp :var i=42 :results silent
  Console.WriteLine(i);
#+end_src"
        (should (= 42 (org-babel-execute-src-block))))))

(ert-deftest test-ob-csharp/float-from-var ()
  "Test of a float variable."
  (test-ob-csharp-with-newest-dotnet
      (org-test-with-temp-text "#+begin_src csharp :var f=3.14 :results silent
  Console.WriteLine(f);
#+end_src"
        (should (= 3.14 (org-babel-execute-src-block))))))

(ert-deftest test-ob-csharp/string-from-var ()
  "Test of a string variable."
  (test-ob-csharp-with-newest-dotnet
      (org-test-with-temp-text "#+begin_src csharp :var s=\"pi\" :results silent
  Console.WriteLine(s);
#+end_src"
        (should (string= "pi" (org-babel-execute-src-block))))))

(ert-deftest test-ob-csharp/outputs-list ()
  "Test list output."
  (test-ob-csharp-with-newest-dotnet
      (org-test-with-temp-text "#+begin_src csharp :results raw list silent
  Console.WriteLine(\"Item 1\");
  Console.WriteLine(\"Item 2\");
  Console.WriteLine(\"Item 3\");
  Console.WriteLine(\"Item 4\");
#+end_src"
        (should (equal "Item 1\nItem 2\nItem 3\nItem 4\n" (org-babel-execute-src-block))))))

(ert-deftest test-ob-csharp/commandline-input ()
  "Test command line input."
  (test-ob-csharp-with-newest-dotnet
      (org-test-with-temp-text "#+begin_src csharp :cmdline 3 :usings '(\"System\" \"System.Text\") :results silent
  int argInt = 0;
  Int32.TryParse(args[0], out argInt);

  Console.WriteLine(argInt * 14);
#+end_src"
        (should (= 42 (org-babel-execute-src-block))))))

(ert-deftest test-ob-csharp/custom-class-and-main ()
  "Test custom class with custom main function."
  (test-ob-csharp-with-newest-dotnet
      (org-test-with-temp-text "#+begin_src csharp :class no :main no :results silent
  internal class ClassA
  {
      public ClassA(int i)
      {
          this.AnInt = i;
      }

      public int AnInt { get; set; }
  }

  public class Program
  {
      public static void Main(string[] args)
      {
          ClassA daInstance = new(123);

          Console.WriteLine(daInstance.AnInt);
      }
  }
#+end_src"
        (should (= 123 (org-babel-execute-src-block))))))

(ert-deftest test-ob-csharp/tabular-format-output ()
  "Test for tabular output format."
  (test-ob-csharp-with-newest-dotnet
      (org-test-with-temp-text "#+begin_src csharp :results table silent
  Console.WriteLine($\"In, questo, mondo, una, cosa\");
  Console.WriteLine($\"si, perde,  una,   si, trova\");
#+end_src"
        (should (equal '(("In" "questo" "mondo" "una" "cosa")
                         ("si" "perde" "una" "si" "trova"))
                       (org-babel-execute-src-block))))))

(ert-deftest test-ob-csharp/nuget-reference ()
  "Test with nuget reference."
  (test-ob-csharp-with-newest-dotnet
      (org-test-with-temp-text "#+begin_src csharp :references '((\"Newtonsoft.Json\" . \"13.0.3\")) :usings '(\"System\" \"Newtonsoft.Json\") :main no :project \"json-test\" :results verbatim silent
  public class DTO
  {
      public int TheInt { get; set; }
      public string TheString { get; set; }
  }

  static void Main(string[] args)
  {
      DTO myDto = new() { TheInt = 12, TheString = \"ok\" };

      string json = JsonConvert.SerializeObject(myDto, Formatting.Indented);
      Console.WriteLine($\"{json}\");
  }
#+end_src"
        (should (string= "{\n  \"TheInt\": 12,\n  \"TheString\": \"ok\"\n}\n"
                         (org-babel-execute-src-block))))))

(ert-deftest test-ob-csharp/respects-custom-nuget-config ()
  "Check that the provided NuGet.config is taken into account when evaluating a source block."
  (test-ob-csharp-with-newest-dotnet
      (let ((nugetconf (make-temp-file "nuget")))
        (unwind-protect
            (progn
              (with-temp-buffer
                (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>
  <configuration>
      <packageSources>
          <clear />
          <add key=\"local\" value=\"./local_packages\" />
      </packageSources>
  </configuration>")
                (write-file nugetconf))
              (org-test-with-temp-text (format "#+begin_src csharp :references '((\"Newtonsoft.Json\" . \"13.0.3\")) :usings '(\"Newtonsoft.Json.Linq\") :nugetconfig %S :results raw
    var js = JObject.Parse(\"{\\\"TheInt\\\": 12, \\\"TheString\\\": \\\"ok\\\"}\");
    Console.Write(js);
  ,#+end_src" nugetconf)
                (should-error (org-babel-execute-src-block))))
          (delete-file nugetconf)))))

(ert-deftest test-ob-csharp/additional-project-flags-fails-with-invalid-syntax ()
  "Compilation fails when the `org-babel-csharp-additional-project-flags' is not xml formatted."
  (test-ob-csharp-with-newest-dotnet
      (unwind-protect
          (progn
            (setq org-babel-csharp-additional-project-flags "somegarbage/>")
            (org-test-with-temp-text "#+begin_src csharp
  Console.WriteLine(\"ok\");
#+end_src"
              (should (eq nil (org-babel-execute-src-block)))))
        (setq org-babel-csharp-additional-project-flags nil))))

(ert-deftest test-ob-csharp/additional-project-flags-executes-with-xml-syntax ()
  "Compilation succeeds when the `org-babel-csharp-additional-project-flags' is xml formatted."
  (test-ob-csharp-with-newest-dotnet
      (unwind-protect
          (progn
            (setq org-babel-csharp-additional-project-flags "<LangVersion>latest</LangVersion>")
            (org-test-with-temp-text "#+begin_src csharp
  Console.WriteLine(\"ok\");
#+end_src"
              (should (string= "ok"
                               (org-babel-execute-src-block)))))
        (setq org-babel-csharp-additional-project-flags nil))))

(ert-deftest test-ob-csharp/prologue-and-epilouge-expanded ()
  "Check if prologue and epilogue are written plain to start and end of the expanded block."
  (test-ob-csharp-with-newest-dotnet
      (org-test-with-temp-text "#+begin_src csharp :prologue \"// File header\" :epilogue \"// file ends here\"
  Console.WriteLine(\"ok\");
#+end_src"
        (let ((block-expand (org-babel-expand-src-block)))
          (should (string= (substring block-expand 0 14) "// File header"))
          (should (string= (substring block-expand -17) "// file ends here"))))))

;; requires at least 2 dotnet frameworks installed
(ert-deftest test-ob-csharp/framework-header-is-configurable ()
  "Check for additional framework header arguments."
  (ert--skip-when (< (length (test-ob-csharp--find-dotnet-version)) 2))
  (let* ((src-result (lambda (v) (org-test-with-temp-text
                                     (format "#+begin_src csharp :framework \"net%s.0\"
  Console.WriteLine(\"ok\");
#+end_src" v)
                                   (org-babel-execute-src-block))))
         (res-first  (funcall src-result (first (test-ob-csharp--find-dotnet-version))))
         (res-second (funcall src-result (second (test-ob-csharp--find-dotnet-version)))))
    (should (string= res-first "ok"))
    (should (string= res-second "ok"))
    (should (string= res-first res-second))
    (should (eq nil (funcall src-result "nonexisting")))))


(provide 'test-ob-csharp)
;;; test-ob-csharp.el ends here
