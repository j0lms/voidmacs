;;; config.el --- Voidmacs config file

;;; Commentary:

;; This file is tangled from ~/.voi.d/org/brain/config.org

;;; Code:

(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
			      :ref nil
			      :files (:defaults (:exclude "extensions"))
			      :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
	(if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
		 ((zerop (call-process "git" nil buffer t "clone"
				       (plist-get order :repo) repo)))
		 ((zerop (call-process "git" nil buffer t "checkout"
				       (or (plist-get order :ref) "--"))))
		 (emacs (concat invocation-directory invocation-name))
		 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
				       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
		 ((require 'elpaca))
		 ((elpaca-generate-autoloads "elpaca" repo)))
	    (progn (message "%s" (buffer-string)) (kill-buffer buffer))
	  (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

(elpaca-wait)

(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")
;;(setq warning-minimum-level :emergency)
(setq server-client-instructions nil)
(eval-and-compile
  (setq gc-cons-threshold 402653184
	gc-cons-percentage 0.6))
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
(setq inhibit-default-init t
      inhibit-startup-echo-area-message t
      inhibit-startup-screen t
      initial-scratch-message nil)
(setq large-file-warning-threshold 100000000)
(defconst gas-savefile-dir (expand-file-name "savefile" user-emacs-directory))
(unless (file-exists-p gas-savefile-dir)
  (make-directory gas-savefile-dir))
(blink-cursor-mode -1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ring-bell-function 'ignore)
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
(setq use-short-answers t)
(setq confirm-nonexistent-file-or-buffer nil)
(delete-selection-mode 1)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
	    kill-buffer-query-functions))
(setq frame-title-format
      '((:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))
(setq-default x-stretch-cursor t)
(setq completion-auto-help nil)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
	       (> (- current (float-time (nth 5 (file-attributes file))))
		  week))
      (message "%s" file)
      (delete-file file))))
(global-auto-revert-mode t)
(setq vc-make-backup-files t)
(setq save-interprogram-paste-before-kill nil)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq kill-whole-line t)
(setq search-default-mode 'char-fold-to-regexp)
(setq global-mark-ring-max 50000)
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
(set 'linuxp (when (eq system-type 'gnu/linux) "yes"))
(set 'windowp (when (eq system-type 'windows-nt) "yes"))
(defun reload-init-file ()
  (interactive)
  (load-file user-init-file)
  (load-file user-init-file))

(defun format-date (format)
  (let ((system-time-locale "es_VE.UTF-8"))
    (insert (format-time-string format))))

(defun insert-deadline ()
  (interactive)
  (format-date "<%Y-%M-%d %A>"))

(defun insert-schedule ()
  (interactive)
  (format-date "<%Y-%m-%d %H:%M>"))

(defun insert-timestamp ()
  (interactive)
  (format-date "[%Y-%m-%d %A %H:%M:%S]"))

(use-package all-the-icons
  :init
  (all-the-icons-install-fonts t)
  :if (display-graphic-p))

(use-package nerd-icons
  :init
  (nerd-icons-install-fonts t))

(use-package which-key
  :init
  (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
	which-key-sort-order #'which-key-key-order-alpha
	which-key-sort-uppercase-first nil
	which-key-add-column-padding 1
	which-key-max-display-columns nil
	which-key-min-display-lines 6
	which-key-side-window-slot -10
	which-key-side-window-max-height 0.25
	which-key-idle-delay 0.5
	which-key-max-description-length 25
	which-key-allow-imprecise-window-fit t
	which-key-separator " → " ))

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  ("C-c n s" . org-journal-new-scheduled-entry)
  :init
  (setq org-journal-date-format "[%Y-%m-%d %A]"
	org-journal-date-prefix "* Entries "
	org-journal-file-format "%Y-%m-%d.org"
	org-journal-dir "~/.voi.d/org/brain/journal"
	org-journal-file-header "#+title: daily\n#+filetags: journal\n"
	org-journal-time-format "[%Y-%m-%d %H:%M:%S] "
	org-journal-enable-agenda-integration t))

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package projectile
  :init
  (projectile-mode 1))

(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers) 
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  (persp-mode))

(use-package magit
  :init
  (with-eval-after-load 'magit-mode
    (add-hook 'after-save-hook 'magit-after-save-refresh-status t)))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
	  treemacs-deferred-git-apply-delay        0.5
	  treemacs-directory-name-transformer      #'identity
	  treemacs-display-in-side-window          t
	  treemacs-eldoc-display                   'simple
	  treemacs-file-event-delay                2000
	  treemacs-file-extension-regex            treemacs-last-period-regex-value
	  treemacs-file-follow-delay               0.2
	  treemacs-file-name-transformer           #'identity
	  treemacs-follow-after-init               t
	  treemacs-expand-after-init               t
	  treemacs-find-workspace-method           'find-for-file-or-pick-first
	  treemacs-git-command-pipe                ""
	  treemacs-goto-tag-strategy               'refetch-index
	  treemacs-header-scroll-indicators        '(nil . "^^^^^^")
	  treemacs-hide-dot-git-directory          t
	  treemacs-indentation                     2
	  treemacs-indentation-string              " "
	  treemacs-is-never-other-window           nil
	  treemacs-max-git-entries                 5000
	  treemacs-missing-project-action          'ask
	  treemacs-move-forward-on-expand          nil
	  treemacs-no-png-images                   nil
	  treemacs-no-delete-other-windows         t
	  treemacs-project-follow-cleanup          nil
	  treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
	  treemacs-position                        'left
	  treemacs-read-string-input               'from-child-frame
	  treemacs-recenter-distance               0.1
	  treemacs-recenter-after-file-follow      nil
	  treemacs-recenter-after-tag-follow       nil
	  treemacs-recenter-after-project-jump     'always
	  treemacs-recenter-after-project-expand   'on-distance
	  treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
	  treemacs-project-follow-into-home        nil
	  treemacs-show-cursor                     nil
	  treemacs-show-hidden-files               t
	  treemacs-silent-filewatch                nil
	  treemacs-silent-refresh                  nil
	  treemacs-sorting                         'alphabetic-asc
	  treemacs-select-when-already-in-treemacs 'move-back
	  treemacs-space-between-root-nodes        t
	  treemacs-tag-follow-cleanup              t
	  treemacs-tag-follow-delay                1.5
	  treemacs-text-scale                      nil
	  treemacs-user-mode-line-format           nil
	  treemacs-user-header-line-format         nil
	  treemacs-wide-toggle-width               70
	  treemacs-width                           35
	  treemacs-width-increment                 1
	  treemacs-width-is-initially-locked       t
	  treemacs-workspace-switch-cleanup        nil)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
		 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
	("M-0"       . treemacs-select-window)
	("C-x t 1"   . treemacs-delete-other-windows)
	("C-x t t"   . treemacs)
	("C-x t d"   . treemacs-select-directory)
	("C-x t B"   . treemacs-bookmark)
	("C-x t C-t" . treemacs-find-file)
	("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-persp
  :after (treemacs persp-mode)
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1)))

(use-package ligature
  :config
  (ligature-set-ligatures 't
			  '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
			    ;; =:= =!=
			    ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
			    ;; ;; ;;;
			    (";" (rx (+ ";")))
			    ;; && &&&
			    ("&" (rx (+ "&")))
			    ;; !! !!! !. !: !!. != !== !~
			    ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
			    ;; ?? ??? ?:  ?=  ?.
			    ("?" (rx (or ":" "=" "\." (+ "?"))))
			    ;; %% %%%
			    ("%" (rx (+ "%")))
			    ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
			    ;; |->>-||-<<-| |- |== ||=||
			    ;; |==>>==<<==<=>==//==/=!==:===>
			    ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
					    "-" "=" ))))
			    ;; \\ \\\ \/
			    ("\\" (rx (or "/" (+ "\\"))))
			    ;; ++ +++ ++++ +>
			    ("+" (rx (or ">" (+ "+"))))
			    ;; :: ::: :::: :> :< := :// ::=
			    (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
			    ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
			    ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
					    "="))))
			    ;; .. ... .... .= .- .? ..= ..<
			    ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
			    ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
			    ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
			    ;; *> */ *)  ** *** ****
			    ("*" (rx (or ">" "/" ")" (+ "*"))))
			    ;; www wwww
			    ("w" (rx (+ "w")))
			    ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
			    ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
			    ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
			    ;; << <<< <<<<
			    ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
					    "-"  "/" "|" "="))))
			    ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
			    ;; >> >>> >>>>
			    (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
			    ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
			    ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
					 (+ "#"))))
			    ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
			    ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
			    ;; __ ___ ____ _|_ __|____|_
			    ("_" (rx (+ (or "_" "|"))))
			    ;; Fira code: 0xFF 0x12
			    ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
			    ;; Fira code:
			    "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
			    ;; The few not covered by the regexps.
			    "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  (global-ligature-mode t))

(use-package pretty-mode
  :init
  (global-pretty-mode t)  
  (add-hook 'my-pretty-language-hook 'turn-on-pretty-mode))

(use-package yasnippet
  :init
  (yas-global-mode t))

(use-package company
  :init
  (global-company-mode t))

(use-package multiple-cursors
  :init
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-x C-<") 'mc/mark-all-like-this))

(use-package undo-tree
  :init
  (global-undo-tree-mode))
(with-eval-after-load 'undo-tree
  (setq undo-tree-auto-save-history nil))

(use-package smart-mode-line-atom-one-dark-theme)
(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'atom-one-dark)
  (sml/setup))
(use-package doom-modeline
  :ensure t
  :init 
  (doom-modeline-mode 1))

(use-package deft
  :bind ("<f8>" . deft)
  :commands (deft)
  :config (setq deft-directory "~/notes"
		deft-extensions '("md" "org")
		deft-recursive t
		deft-use-filename-as-title t))

(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package beacon
  :init
  (beacon-mode 1))

(use-package hl-todo
  :init
  (global-hl-todo-mode))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (global-set-key (kbd "C-c d") '(lambda () (interactive) (dashboard-open)))
  (setq dashboard-banner-logo-title "")
  (setq dashboard-center-content t)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-items '((recents  . 5)
			  (bookmarks . 5)
			  (projects . 5)
			  (agenda . 5)))
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-startup-banner '("~/.voi.d/banners/blackhole-lines.svg" . "~/.voi.d/banners/blackhole-lines.txt"))
  (setq dashboard-item-names '(("Recent Files:" . "  RECENT")
			       ("Bookmarks:" . "  BOOKMARKS")
			       ("Projects:" . "  PROJECTS")
			       ("Agenda for the coming week:" . "  AGENDA")))
  (setq dashboard-footer-messages '(
				    "voidmacs"
				    ))
  (setq dashboard-projects-switch-function 'projectile-persp-switch-project))
  ;; (setq dashboard-set-navigator t)
  ;; (setq dashboard-navigator-buttons
  ;; 	`(

  ;; 	  ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
  ;; 	    "Homepage"
  ;; 	    "Browse homepage"
  ;; 	    (lambda (&rest _) (browse-url "homepage")))
  ;; 	   ("★" "Star" "Show stars" (lambda (&rest _) (show-stars)) warning)
  ;; 	   ("?" "" "?/h" #'show-help nil "<" ">"))

  ;; 	  ((,(all-the-icons-faicon "linkedin" :height 1.1 :v-adjust 0.0)
  ;; 	    "Linkedin"
  ;; 	    ""
  ;; 	    (lambda (&rest _) (browse-url "homepage")))
  ;; 	   ("⚑" nil "Show flags" (lambda (&rest _) (message "flag")) error)))))

(use-package ace-popup-menu
  :init
  (ace-popup-menu-mode 1))

(use-package pulsar
  :init
  (pulsar-global-mode t)
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-highlight-face 'pulsar-yellow)
  (let ((map global-map))
    (define-key map (kbd "C-c h p") #'pulsar-pulse-line)
    (define-key map (kbd "C-c h h") #'pulsar-highlight-line))
  (add-hook 'next-error-hook #'pulsar-pulse-line)
  (add-hook 'imenu-after-jump-hook #'pulsar-recenter-top)
  (add-hook 'imenu-after-jump-hook #'pulsar-reveal-entry))

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(use-package minimap
  :init
  (setq minimap-window-location 'right)
  (global-set-key (kbd "C-c m") '(lambda () (interactive) (minimap-mode))))

(use-package unicode-fonts
  :init
  (unicode-fonts-setup))

(use-package amx
  :init
  (amx-mode 1))

(use-package ido-completing-read+
  :init
  (ido-ubiquitous-mode 1))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (
	 (python-mode . lsp-deferred)
	 (sh-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))
(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package elpher)

(use-package elfeed)

(use-package solaire-mode
  :init
  (solaire-global-mode +1))

(use-package indent-guide
  :init
  (indent-guide-global-mode))

(use-package emamux
  :bind
  ("C-c c" . emamux:send-region))

(use-package pdf-tools)

(use-package smartparens
  :init
  (smartparens-global-mode +1))

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :init
  (setq org-modern-label-border 'auto
	org-modern-star nil
	org-modern-hide-star nil
	org-modern-block-name nil
	org-modern-keyword nil
	org-modern-timestamp t
	org-modern-checkbox nil))

;; (defun org-babel-execute:xonsh (body params)
;;   (let ((in-file (org-babel-temp-file "xonsh-")))
;;    (with-temp-file in-file
;;       (insert body))
;;     (org-babel-eval
;;      (format "echo -e $($HOME/.pyenv/versions/3.11.4/bin/xonsh -c $(cat %s | sed \"s/^[[:space:]]*//g\"  | awk \"{print $1}\")) | sed \"s/\x1b\\[38;5;241m//g\" | sed \"s/\x1b\\[39m//g\" | sed -z \"/\n/d\" "
;;        (org-babel-process-file-name in-file))
;;      "")))))
(use-package xonsh-mode)

(use-package org-brain :ensure t
  :init
  (setq org-brain-path "~/.voi.d/org/brain")
  (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.voi.d/org/brain/ids/.org-id-locations")
  (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
  (add-hook 'org-brain-visualize-mode-hook #'org-mode)
  (display-line-numbers-mode 0)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12)
  (setq org-brain-include-file-entries t
	org-brain-file-entries-use-title nil))

(use-package pyenv-mode
  :init
  (pyenv-mode))

(add-hook 'eshell-mode-hook 'my-inhibit-global-linum-mode)

(add-hook 'term-mode-hook 'my-inhibit-global-linum-mode)

(defun my-inhibit-global-linum-mode ()
  (add-hook 'after-change-major-mode-hook
	    (lambda () (display-line-numbers-mode 0))
	    :append :local))

(use-package workgroups2
  :init 
  (workgroups-mode 1)
  (setq wg-session-file "~/.voi.d/.emacs_workgroups")
  :bind
  ("C-c z" . wg-prefix-key))

(use-package origami
  :init
  (global-origami-mode 1)
  :bind
  ("C-<tab>" . origami-recursively-toggle-node))

(use-package highlight-defined
:init
(add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode)
(add-hook 'org-mode-hook 'highlight-defined-mode))

(use-package elmacro
  :init
  (elmacro-mode))

(use-package eros
  :init
  (eros-mode 1))

(windmove-default-keybindings)
(setq tab-always-indent 'complete)
(global-set-key (kbd "C-c t") '(lambda () (interactive) (eshell)))
(global-set-key (kbd "C-c f") '(lambda () (interactive) (find-file "~/.voi.d/org/brain/config.org")))
(global-set-key (kbd "C-c x") '(lambda () (interactive) (find-file "~/.voi.d/org/brain/xonsh.org")))
(global-set-key (kbd "C-c s") '(lambda () (interactive) (find-file "~/.voi.d/org/brain/bash.org")))
(global-set-key (kbd "C-c i") '(lambda () (interactive) (find-file "~/.voi.d/org/brain/init.org")))
(global-set-key (kbd "C-x a") '(lambda () (interactive) (insert-timestamp)))
(global-set-key (kbd "C-x C-l") '(lambda () (interactive) (org-toggle-timestamp-type)))
(global-set-key (kbd "C-x s") '(lambda () (interactive) (save-buffer)))

(set-face-attribute 'default t
		    :font "Fira Code"
		    :height 110
		    :weight 'medium)
(set-face-attribute 'variable-pitch t
		    :font "Lato"
		    :height 110
		    :weight 'medium)
(set-face-attribute 'fixed-pitch t
		    :font "JetBrains Mono"
		    :height 110
		    :weight 'medium)
(set-face-attribute 'font-lock-comment-face t
		    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face t
		    :slant 'italic)

(add-to-list 'default-frame-alist '(font . "JetBrains Mono-11"))

(setq-default line-spacing 0.12)

(setq org-directory "~/.voi.d/org/")
(setq org-agenda-files (quote ("~/.voi.d/org/brain/agenda/")))
(setq org-catch-invisible-edits 'show-and-error)
(setq org-special-ctrl-a/e t)
(setq org-insert-heading-respect-content t)
(add-hook 'after-save-hook (lambda () (org-babel-tangle)))
(org-babel-tangle-file "~/.voi.d/org/brain/xonsh.org")
(org-babel-tangle-file "~/.voi.d/org/brain/bash.org")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (lilypond . t)))
(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("emacs-lisp" "python" "bash" "lilypond" "xonsh"))))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
(electric-indent-mode -1)
(defun org-icons ()
  (setq prettify-symbols-alist '(("#+begin_src" . "❱")
				 ("#+end_src" . "❰")
				 ("#+RESULTS:" . "∴")
				 ("#+begin_example" . "⋉")
				 ("#+end_example" . "⋊")
				 (":PROPERTIES:" . "")
				 (":ID:" . "") ;; ??
				 (":END:" . "―")
				 ("#+startup:" . "")
				 ("#+title:" . "")
				 ("#+author:" . "")
				 ("#+header:" . "")
				 ("#+name:" . "")
				 ("#+filetags:" . "")
				 ("#+description:" . "")
				 ("#+subtitle:" . "⛛")
				 ("#+options:" . "⚒")
				 ("[ ]" . "")
				 ("[X]" . "")
				 ("[-]" . ""))))
(add-hook 'org-mode-hook 'org-icons)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode t)
(size-indication-mode t)
(global-display-line-numbers-mode 0)
(global-visual-line-mode t)
(global-prettify-symbols-mode t)

(provide 'config)
;;; init.el ends here
