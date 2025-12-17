;;; msbuild.el --- Windows C/C++ build env setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Commands to set environment variables MSVC needs like INCLUDE, LIB,
;; LIBPATH and PATH properly with correct paths.

;;; Code:

(require 'treesit)

(declare-function msbuild-2022-x86-setup "msbuild")
(declare-function msbuild-2022-x64-setup "msbuild")
(declare-function msbuild--compile-grammar-msvc "msbuild")

(when (eq system-type 'windows-nt)
  (defvar msbuild-old-path-var (getenv "PATH"))

  (defconst msbuild-msvc-version "14.44.35207"
    "The specific MSVC compiler toolset version.")
  (defconst msbuild-win-kit-version "10.0.26100.0"
    "The specific Windows SDK version.")

  (defun msbuild-2022-x86-setup ()
    "Set environment variables to load Microsoft Visual C++ Compiler (MSVC 32 bits)."
    (interactive)
    (message "Setting 32 bits MSVC building tools.")
    (setenv "PATH" msbuild-old-path-var)

    ;; --- INCLUDE ---
    (setenv
     "INCLUDE"
     (concat
      (format
       "C:/Program Files (x86)/Microsoft Visual Studio/2022/BuildTools/VC/Tools/MSVC/%s/include"
       msbuild-msvc-version)
      ";"
      (format "C:/Program Files (x86)/Windows Kits/10/Include/%s/ucrt"
              msbuild-win-kit-version)
      ";"
      (format
       "C:/Program Files (x86)/Windows Kits/10/Include/%s/shared"
       msbuild-win-kit-version)
      ";"
      (format "C:/Program Files (x86)/Windows Kits/10/Include/%s/um"
              msbuild-win-kit-version)
      ";"
      (format
       "C:/Program Files (x86)/Windows Kits/10/Include/%s/winrt"
       msbuild-win-kit-version)))

    ;; --- LIB ---
    (setenv
     "LIB"
     (concat
      (format
       "C:/Program Files (x86)/Microsoft Visual Studio/2022/BuildTools/VC/Tools/MSVC/%s/lib/x86"
       msbuild-msvc-version)
      ";"
      (format "C:/Program Files (x86)/Windows Kits/10/lib/%s/ucrt/x86"
              msbuild-win-kit-version)
      ";"
      (format "C:/Program Files (x86)/Windows Kits/10/lib/%s/um/x86"
              msbuild-win-kit-version)))

    ;; --- LIBPATH ---
    (setenv
     "LIBPATH"
     (concat
      (format
       "C:/Program Files (x86)/Microsoft Visual Studio/2022/BuildTools/VC/Tools/MSVC/%s/lib/x86"
       msbuild-msvc-version)
      ";"
      (format
       "C:/Program Files (x86)/Microsoft Visual Studio/2022/BuildTools/VC/Tools/MSVC/%s/lib/x86/store/references"
       msbuild-msvc-version)
      ";"
      (format
       "C:/Program Files (x86)/Windows Kits/10/UnionMetadata/%s"
       msbuild-win-kit-version)
      ";"
      (format "C:/Program Files (x86)/Windows Kits/10/References/%s"
              msbuild-win-kit-version)))

    ;; --- PATH ---
    (setenv
     "PATH"
     (concat
      (getenv "PATH")
      ";"
      (format
       "C:/Program Files (x86)/Microsoft Visual Studio/2022/BuildTools/VC/Tools/MSVC/%s/bin/HostX86/x86"
       msbuild-msvc-version)
      ";"
      "C:/Program Files (x86)/Microsoft Visual Studio/2022/BuildTools/Common7/IDE/VC/VCPackages"
      ";"
      "C:/Program Files (x86)/Microsoft Visual Studio/2022/BuildTools/Common7/IDE/CommonExtensions/Microsoft/TestWindow"
      ";"
      "C:/Program Files (x86)/Microsoft Visual Studio/2022/BuildTools/MSBuild/Current/Bin/Roslyn"
      ";"
      "C:/Program Files (x86)/Windows Kits/10/bin/x86"
      ";"
      (format "C:/Program Files (x86)/Windows Kits/10/bin/%s/x86"
              msbuild-win-kit-version)
      ";"
      "C:/Program Files (x86)/Microsoft Visual Studio/2022/BuildTools/MSBuild/Current/bin"
      ";"
      "C:/Program Files (x86)/Microsoft Visual Studio/2022/BuildTools/Common7/IDE/"
      ";"
      "C:/Program Files (x86)/Microsoft Visual Studio/2022/BuildTools/Common7/Tools/")))


  (defun msbuild-2022-x64-setup ()
    "Set environment variables to load Microsoft Visual C++ Compiler (MSVC 64 bits)."
    (interactive)
    (message "Setting 64 bits MSVC building tools.")
    (setenv "PATH" msbuild-old-path-var) ; Reset PATH

    ;; --- INCLUDE --- (Paths are the same as x86 for includes)
    (setenv
     "INCLUDE"
     (concat
      (format
       "C:/Program Files (x86)/Microsoft Visual Studio/2022/BuildTools/VC/Tools/MSVC/%s/include"
       msbuild-msvc-version)
      ";"
      (format "C:/Program Files (x86)/Windows Kits/10/Include/%s/ucrt"
              msbuild-win-kit-version)
      ";"
      (format
       "C:/Program Files (x86)/Windows Kits/10/Include/%s/shared"
       msbuild-win-kit-version)
      ";"
      (format "C:/Program Files (x86)/Windows Kits/10/Include/%s/um"
              msbuild-win-kit-version)
      ";"
      (format
       "C:/Program Files (x86)/Windows Kits/10/Include/%s/winrt"
       msbuild-win-kit-version)))

    ;; --- LIB ---
    (setenv
     "LIB"
     (concat
      (format
       "C:/Program Files (x86)/Microsoft Visual Studio/2022/BuildTools/VC/Tools/MSVC/%s/lib/x64"
       msbuild-msvc-version)
      ";"
      (format "C:/Program Files (x86)/Windows Kits/10/lib/%s/ucrt/x64"
              msbuild-win-kit-version)
      ";"
      (format "C:/Program Files (x86)/Windows Kits/10/lib/%s/um/x64"
              msbuild-win-kit-version)))

    ;; --- LIBPATH ---
    (setenv
     "LIBPATH"
     (concat
      (format
       "C:/Program Files (x86)/Microsoft Visual Studio/2022/BuildTools/VC/Tools/MSVC/%s/lib/x64"
       msbuild-msvc-version)
      ";"
      (format
       "C:/Program Files (x86)/Microsoft Visual Studio/2022/BuildTools/VC/Tools/MSVC/%s/lib/x64/store/references"
       msbuild-msvc-version)
      ";"
      (format
       "C:/Program Files (x86)/Windows Kits/10/UnionMetadata/%s"
       msbuild-win-kit-version)
      ";"
      (format "C:/Program Files (x86)/Windows Kits/10/References/%s"
              msbuild-win-kit-version)))

    ;; --- PATH ---
    (setenv
     "PATH"
     (concat
      (getenv "PATH")
      ";"
      (format
       "C:/Program Files (x86)/Microsoft Visual Studio/2022/BuildTools/VC/Tools/MSVC/%s/bin/HostX86/x64"
       msbuild-msvc-version)
      ";"
      "C:/Program Files (x86)/Microsoft Visual Studio/2022/BuildTools/Common7/IDE/VC/VCPackages"
      ";"
      "C:/Program Files (x86)/Microsoft Visual Studio/2022/BuildTools/Common7/IDE/CommonExtensions/Microsoft/TestWindow"
      ";"
      "C:/Program Files (x86)/Microsoft Visual Studio/2022/BuildTools/MSBuild/Current/Bin/Roslyn"
      ";"
      "C:/Program Files (x86)/Windows Kits/10/bin/x64"
      ";"
      (format "C:/Program Files (x86)/Windows Kits/10/bin/%s/x64"
              msbuild-win-kit-version)
      ";"
      "C:/Program Files (x86)/Microsoft Visual Studio/2022/BuildTools/MSBuild/Current/bin"
      ";"
      "C:/Program Files (x86)/Microsoft Visual Studio/2022/BuildTools/Common7/IDE/"
      ";"
      "C:/Program Files (x86)/Microsoft Visual Studio/2022/BuildTools/Common7/Tools/")))

  (defun compile-msvc-x86 ()
    "Set 32 bits build env for Windows C/C++ 2022 workload and run compile."
    (interactive)
    (msbuild-2022-x86-setup)
    (let ((compile-command
           (format "cl.exe /EHsc \"%s\""
                   (file-name-nondirectory (buffer-file-name))))
          (compilation-ask-about-save nil))
      (call-interactively #'compile)))

  (defun compile-msvc-x64 ()
    "Set 64 bits build env for Windows C/C++ 2022 workload and run compile."
    (interactive)
    (msbuild-2022-x64-setup)
    (let ((compile-command
           (format "cl.exe /EHsc \"%s\""
                   (file-name-nondirectory (buffer-file-name))))
          (compilation-ask-about-save nil))
      (call-interactively #'compile)))

  (defun compile-msvc-x86-debug ()
    "Set 32 bits build env for Windows C/C++ 2022 workload and run debug compile.
Uses /EHsc /Zi /Od flags. Output is named <filename>_d.exe."
    (interactive)
    (msbuild-2022-x86-setup)
    (let* ((filename
            (file-name-sans-extension
             (file-name-nondirectory (buffer-file-name))))
           (compile-command
            (format
             "cl.exe /EHsc /Zi /Od /Fe%s_d.exe \"%s\""
             filename (file-name-nondirectory (buffer-file-name))))
           (compilation-ask-about-save nil))
      (call-interactively #'compile)))

  (defun compile-msvc-x64-debug ()
    "Set 64 bits build env for Windows C/C++ 2022 workload and run debug compile.
Uses /EHsc /Zi /Od flags. Output is named <filename>_d.exe."
    (interactive)
    (msbuild-2022-x64-setup)
    (let* ((filename
            (file-name-sans-extension
             (file-name-nondirectory (buffer-file-name))))
           (compile-command
            (format
             "cl.exe /EHsc /Zi /Od /Fe%s_d.exe \"%s\""
             filename (file-name-nondirectory (buffer-file-name))))
           (compilation-ask-about-save nil))
      (call-interactively #'compile)))

  (defun compile-msvc-x86-dll ()
    "Set 32 bits build env for Windows C/C++ 2022 workload and compile a DLL.
Uses /LD /EHsc flags. Output is named <filename>.dll."
    (interactive)
    (msbuild-2022-x86-setup)
    (let* ((filename
            (file-name-sans-extension
             (file-name-nondirectory (buffer-file-name))))
           (compile-command
            (format
             "cl.exe /LD /EHsc /Fe%s.dll \"%s\""
             filename (file-name-nondirectory (buffer-file-name))))
           (compilation-ask-about-save nil))
      (call-interactively #'compile)))

  (defun compile-msvc-x64-dll ()
    "Set 64 bits build env for Windows C/C++ 2022 workload and compile a DLL.
Uses /LD /EHsc flags. Output is named <filename>.dll."
    (interactive)
    (msbuild-2022-x64-setup)
    (let* ((filename
            (file-name-sans-extension
             (file-name-nondirectory (buffer-file-name))))
           (compile-command
            (format
             "cl.exe /LD /EHsc /Fe%s.dll \"%s\""
             filename (file-name-nondirectory (buffer-file-name))))
           (compilation-ask-about-save nil))
      (call-interactively #'compile)))

  (defun compile-msvc-x86-debug-dll ()
    "Set 32 bits build env for Windows C/C++ 2022 workload and compile a debug DLL.
Uses /LD /EHsc /Zi /Od flags. Output is named <filename>_d.dll."
    (interactive)
    (msbuild-2022-x86-setup)
    (let* ((filename
            (file-name-sans-extension
             (file-name-nondirectory (buffer-file-name))))
           (compile-command
            (format
             "cl.exe /LD /EHsc /Zi /Od /Fe%s_d.dll \"%s\""
             filename (file-name-nondirectory (buffer-file-name))))
           (compilation-ask-about-save nil))
      (call-interactively #'compile)))

  (defun compile-msvc-x64-debug-dll ()
    "Set 64 bits build env for Windows C/C++ 2022 workload and compile a debug DLL.
Uses /LD /EHsc /Zi /Od flags. Output is named <filename>_d.dll."
    (interactive)
    (msbuild-2022-x64-setup)
    (let* ((filename
            (file-name-sans-extension
             (file-name-nondirectory (buffer-file-name))))
           (compile-command
            (format
             "cl.exe /LD /EHsc /Zi /Od /Fe%s_d.dll \"%s\""
             filename (file-name-nondirectory (buffer-file-name))))
           (compilation-ask-about-save nil))
      (call-interactively #'compile)))

  (defun msbuild--compile-grammar-msvc (lang source-dir out-dir)
    "Compile the LANG grammar in SOURCE-DIR to OUT-DIR using cl.exe with MSVC flags."
    (let* ((default-directory source-dir)
           (out-file (format "libtree-sitter-%s.dll" lang))
           (final-dest (expand-file-name out-file out-dir))
           (scanner-c (expand-file-name "scanner.c" source-dir))
           (scanner-cc (expand-file-name "scanner.cc" source-dir))
           (srcs
            (cons
             "parser.c"
             (cond
              ((file-exists-p scanner-c)
               (list "scanner.c"))
              ((file-exists-p scanner-cc)
               (list "scanner.cc"))
              (t
               nil))))
           (args
            (append
             '("/nologo" "/LD" "/O2" "/Utf-8" "/I.")
             srcs
             (list (format "/Fe:%s" out-file)))))
      (message "Compiling %s with MSVC..." lang)
      (unless (eq 0 (apply #'call-process "cl.exe" nil t nil args))
        (error
         "MSVC compilation failed. Check the *Messages* buffer for output"))
      (unless (file-exists-p out-dir)
        (make-directory out-dir t))
      (if (file-exists-p final-dest)
          (condition-case nil
              (rename-file final-dest (concat final-dest ".old") t)
            (error
             nil)))
      (copy-file out-file final-dest t)
      (mapc
       (lambda (f)
         (when (file-exists-p f)
           (delete-file f)))
       (directory-files "." nil "\\.\\(obj\\|lib\\|exp\\|dll\\)$"))
      (message "Installed %s to %s" out-file out-dir)))

  (defun msbuild-install-tree-sitter-grammar (lang)
    "Build and install a tree-sitter grammar using the msbuild.el environment.
Uses recipes from `treesit-language-source-alist'."
    (interactive (list
                  (intern
                   (completing-read
                    "Language: "
                    (mapcar #'car treesit-language-source-alist)))))
    (let ((recipe (alist-get lang treesit-language-source-alist)))
      (unless recipe
        (error
         "No recipe found for %s in `treesit-language-source-alist'"
         lang))
      (let* ((url (nth 0 recipe))
             (revision (nth 1 recipe))
             (source-subdir (or (nth 2 recipe) "src"))
             (install-dir (locate-user-emacs-file "tree-sitter"))
             (workdir (make-temp-file "treesit-work" t))
             (original-exec-path exec-path))
        (unwind-protect
            (progn
              (msbuild-2022-x64-setup)
              (setq exec-path
                    (split-string (getenv "PATH") path-separator))
              (message "Cloning %s..." url)
              (if revision
                  (call-process "git" nil nil nil "clone" "--depth" "1" "-b"
				revision url workdir)
                (call-process "git"nil nil nil "clone" "--depth" "1" url workdir))
              (let ((full-source-dir
                     (expand-file-name source-subdir workdir)))
                (msbuild--compile-grammar-msvc
                 lang full-source-dir install-dir)))
          (delete-directory workdir t))))))

(provide 'msbuild)
;;; msbuild.el ends here
