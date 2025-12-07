;;; msbuild.el --- Windows C/C++ build env setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Commands to set environment variables MSVC needs like INCLUDE, LIB,
;; LIBPATH and PATH properly with correct paths.

;;; Code:

(declare-function msbuild-2022-x86-setup "msbuild")
(declare-function msbuild-2022-x64-setup "msbuild")

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
      (call-interactively #'compile))))

(provide 'msbuild)
;;; msbuild.el ends here
