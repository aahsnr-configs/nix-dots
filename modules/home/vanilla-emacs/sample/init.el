;;; init.el --- Emacs Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; A production-ready Emacs configuration managed with Nix + Home Manager
;; All packages are managed declaratively in emacs.nix
;; This file only contains configuration, not package installation

;;; Code:

;; ============================================================================
;; Performance & Startup Optimization
;; ============================================================================

;; Set custom file (prevents Emacs from modifying init.el)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Garbage collection optimization
(setq gc-cons-threshold (* 50 1000 1000))  ; 50MB during runtime

;; Increase data read from processes (important for LSP)
(setq read-process-output-max (* 1024 1024))  ; 1MB

;; ============================================================================
;; Personal Information
;; ============================================================================

(setq user-full-name "Your Name"
      user-mail-address "your.email@example.com")

;; ============================================================================
;; Use-package Setup
;; ============================================================================

(require 'use-package)
(setq use-package-always-defer t          ; Lazy load by default
      use-package-expand-minimally t      ; Reduce macro expansion
      use-package-compute-statistics nil) ; Don't track loading times

;; ============================================================================
;; Basic Settings
;; ============================================================================

(use-package emacs
  :demand t
  :custom
  ;; Startup
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  
  ;; Annoyances
  (ring-bell-function 'ignore)
  (use-dialog-box nil)
  (use-file-dialog nil)
  
  ;; Display
  (display-line-numbers-type 'relative)
  (fill-column 80)
  (truncate-lines t)
  (word-wrap t)
  
  ;; Scrolling
  (scroll-conservatively 101)
  (scroll-margin 3)
  (scroll-preserve-screen-position t)
  (auto-window-vscroll nil)
  
  ;; Indentation
  (indent-tabs-mode nil)
  (tab-width 4)
  (standard-indent 2)
  
  ;; Backups and auto-saves
  (backup-by-copying t)
  (backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory))))
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t)
  (auto-save-default t)
  (auto-save-timeout 20)
  (auto-save-interval 200)
  
  ;; Better default behaviors
  (require-final-newline t)
  (sentence-end-double-space nil)
  (save-interprogram-paste-before-kill t)
  (enable-recursive-minibuffers t)
  (tab-always-indent 'complete)
  
  :config
  ;; UI cleanup
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode -1)
  
  ;; Mode line
  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode t)
  
  ;; Display line numbers in programming modes
  (global-display-line-numbers-mode t)
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  shell-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))
  
  ;; UTF-8 everywhere
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  
  ;; Better buffer names for duplicates
  (setq uniquify-buffer-name-style 'forward)
  
  ;; Remember cursor position
  (save-place-mode 1)
  
  ;; Recent files
  (recentf-mode 1)
  (setq recentf-max-menu-items 25
        recentf-max-saved-items 100))

;; ============================================================================
;; Theme & Appearance
;; ============================================================================

(use-package doom-themes
  :demand t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one t)
  
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; Corrects (and improves) org-mode's native fontification
  (doom-themes-org-config))

;; Alternative theme option: nano-theme
;; (use-package nano-theme
;;   :demand t
;;   :config
;;   (load-theme 'nano-light t))

(use-package doom-modeline
  :demand t
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-env-version t)
  (doom-modeline-github nil)
  (doom-modeline-github-interval (* 30 60))
  :config
  (doom-modeline-mode 1))

(use-package all-the-icons
  :demand t
  :if (display-graphic-p))

(use-package nerd-icons
  :if (display-graphic-p))

;; ============================================================================
;; Keybinding Helpers
;; ============================================================================

(use-package which-key
  :demand t
  :custom
  (which-key-idle-delay 0.3)
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'bottom)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-min-display-lines 5)
  :config
  (which-key-mode 1))

(use-package general
  :demand t
  :config
  ;; Set up leader key
  (general-create-definer leader-def
    :prefix "C-c"))

;; ============================================================================
;; Completion Framework (Vertico + Consult + Embark)
;; ============================================================================

(use-package vertico
  :demand t
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-count 15)
  :config
  (vertico-mode 1))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :demand t
  :custom
  (marginalia-align 'right)
  :config
  (marginalia-mode 1))

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :custom
  (consult-narrow-key "<")
  (consult-project-function #'projectile-project-root)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any)))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; ============================================================================
;; Completion at Point (Corfu)
;; Alternative: use company instead of corfu by commenting this section
;; ============================================================================

(use-package corfu
  :demand t
  :custom
  (corfu-auto t)                      ; Enable auto completion
  (corfu-auto-delay 0.2)              ; Delay for auto completion
  (corfu-auto-prefix 2)               ; Minimum prefix length for auto completion
  (corfu-cycle t)                     ; Enable cycling for `corfu-next/previous'
  (corfu-quit-no-match 'separator)    ; Don't quit if there is no match
  (corfu-preselect 'prompt)           ; Preselect the prompt
  (corfu-scroll-margin 5)             ; Margin for scrolling
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous))
  :config
  (global-corfu-mode 1))

(use-package cape
  :after corfu
  :init
  ;; Add completion backends
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  :config
  ;; Super cape for multiple completions
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

;; Alternative: Company Mode (comment out Corfu section above if using this)
;; (use-package company
;;   :demand t
;;   :custom
;;   (company-idle-delay 0.2)
;;   (company-minimum-prefix-length 2)
;;   (company-tooltip-align-annotations t)
;;   (company-require-match 'never)
;;   :bind (:map company-active-map
;;               ("C-n" . company-select-next)
;;               ("C-p" . company-select-previous))
;;   :config
;;   (global-company-mode 1))

;; ============================================================================
;; Programming: Language Server Protocol (Eglot)
;; ============================================================================

(use-package eglot
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (nix-mode . eglot-ensure)
         (nix-ts-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t)              ; Shutdown server when last buffer is killed
  (eglot-events-buffer-size 0)        ; Disable event logging for performance
  (eglot-sync-connect nil)            ; Don't block on connection
  (eglot-connect-timeout 10)          ; Timeout for connection
  :config
  ;; Customize server programs if needed
  (add-to-list 'eglot-server-programs
               '(nix-mode . ("nil")))
  (add-to-list 'eglot-server-programs
               '(nix-ts-mode . ("nil")))
  
  ;; Optimize for performance
  (fset #'jsonrpc--log-event #'ignore)
  
  ;; Keybindings
  (leader-def
    :keymaps 'eglot-mode-map
    "l a" 'eglot-code-actions
    "l r" 'eglot-rename
    "l f" 'eglot-format
    "l d" 'eldoc-doc-buffer))

;; ============================================================================
;; Programming: Syntax Checking (Flycheck)
;; ============================================================================

(use-package flycheck
  :demand t
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-display-errors-delay 0.3)
  :config
  (global-flycheck-mode 1))

(use-package flycheck-inline
  :after flycheck
  :config
  (global-flycheck-inline-mode 1))

;; ============================================================================
;; Programming: Snippets
;; ============================================================================

(use-package yasnippet
  :demand t
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

;; ============================================================================
;; Programming: Tree-sitter
;; ============================================================================

(use-package treesit
  :when (and (fboundp 'treesit-available-p)
             (treesit-available-p))
  :custom
  (treesit-font-lock-level 4)
  :config
  ;; Remap major modes to tree-sitter variants
  (setq major-mode-remap-alist
        '((python-mode . python-ts-mode)
          (javascript-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (rust-mode . rust-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode))))

;; ============================================================================
;; Programming: Languages
;; ============================================================================

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :if (treesit-ready-p 'nix))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "multimarkdown"))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package json-mode
  :mode "\\.json\\'")

(use-package rust-mode
  :mode "\\.rs\\'"
  :custom
  (rust-format-on-save t))

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package haskell-mode
  :mode "\\.hs\\'")

;; ============================================================================
;; Version Control: Magit
;; ============================================================================

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c g" . magit-file-dispatch))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-refine-hunk t)
  :config
  (leader-def
    "g" 'magit-status))

(use-package diff-hl
  :demand t
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1)
  (diff-hl-margin-mode 1))

(use-package git-timemachine
  :bind ("C-x v t" . git-timemachine))

(use-package forge
  :after magit
  :config
  (leader-def
    :keymaps 'forge-topic-mode-map
    "c" 'forge-create-issue
    "b" 'forge-browse-topic))

;; ============================================================================
;; Project Management: Projectile
;; ============================================================================

(use-package projectile
  :demand t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'default)
  (projectile-enable-caching t)
  (projectile-indexing-method 'alien)
  (projectile-sort-order 'recentf)
  (projectile-switch-project-action #'projectile-dired)
  :config
  (projectile-mode 1)
  (leader-def
    "p" 'projectile-command-map))

;; ============================================================================
;; Navigation & Editing
;; ============================================================================

(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1))
  :custom
  (avy-background t)
  (avy-style 'at-full))

(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame))

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package undo-tree
  :demand t
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist
   `(("." . ,(expand-file-name "undo-tree-hist" user-emacs-directory))))
  :config
  (global-undo-tree-mode 1))

;; ============================================================================
;; UI Enhancements
;; ============================================================================

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top))

;; ============================================================================
;; Org Mode
;; ============================================================================

(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :custom
  (org-directory "~/org")
  (org-agenda-files (list org-directory))
  (org-default-notes-file (expand-file-name "notes.org" org-directory))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis " ▾")
  (org-agenda-start-with-log-mode t)
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "|" "DONE(d!)")
     (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CANC(c@/!)")))
  :config
  ;; Org babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)))
  
  ;; Don't ask before evaluating code blocks
  (setq org-confirm-babel-evaluate nil)
  
  ;; Better source block editing
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0))

(use-package org-modern
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶"))
  (org-modern-table-vertical 1)
  (org-modern-table-horizontal 0.2))

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t))

(use-package org-roam
  :after org
  :custom
  (org-roam-directory (file-truename "~/org/roam"))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n g" . org-roam-graph))
  :config
  (org-roam-db-autosync-mode 1))

(use-package org-super-agenda
  :after org-agenda
  :custom
  (org-super-agenda-groups
   '((:name "Today"
            :time-grid t
            :scheduled today)
     (:name "Important"
            :priority "A")
     (:name "Due soon"
            :deadline future)
     (:name "Overdue"
            :deadline past)
     (:name "Meetings"
            :tag "meeting")
     (:name "Projects"
            :tag "project")))
  :config
  (org-super-agenda-mode 1))

;; ============================================================================
;; Utilities
;; ============================================================================

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-c C-d" . helpful-at-point)))

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

(use-package editorconfig
  :demand t
  :config
  (editorconfig-mode 1))

(use-package envrc
  :demand t
  :config
  (envrc-global-mode 1))

;; macOS: Import PATH from shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :demand t
  :config
  (exec-path-from-shell-initialize))

;; ============================================================================
;; Dired Enhancements
;; ============================================================================

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  :config
  ;; Automatically refresh dired buffers
  (add-hook 'dired-mode-hook 'auto-revert-mode))

;; ============================================================================
;; Terminal Integration
;; ============================================================================

(use-package vterm
  :if (and module-file-suffix (executable-find "cmake"))
  :bind ("C-c t" . vterm)
  :custom
  (vterm-max-scrollback 10000)
  (vterm-kill-buffer-on-exit t))

;; ============================================================================
;; Custom Functions
;; ============================================================================

(defun my/reload-init-file ()
  "Reload init.el."
  (interactive)
  (load-file user-init-file)
  (message "Reloaded init.el"))

(defun my/edit-init-file ()
  "Open init.el for editing."
  (interactive)
  (find-file user-init-file))

(defun my/open-config-dir ()
  "Open Emacs configuration directory."
  (interactive)
  (find-file user-emacs-directory))

(defun my/insert-current-date ()
  "Insert current date in ISO format."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun my/insert-current-time ()
  "Insert current time."
  (interactive)
  (insert (format-time-string "%H:%M:%S")))

;; Keybindings for custom functions
(leader-def
  "f e d" 'my/edit-init-file
  "f e r" 'my/reload-init-file
  "f e c" 'my/open-config-dir
  "i d" 'my/insert-current-date
  "i t" 'my/insert-current-time)

;; ============================================================================
;; Final Cleanup
;; ============================================================================

;; Restore garbage collection to reasonable level
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold (* 2 1000 1000))
    (message "Emacs loaded in %.2f seconds with %d garbage collections."
             (float-time (time-subtract after-init-time before-init-time))
             gcs-done)))

;; ============================================================================
;; Provide
;; ============================================================================

(provide 'init)
;;; init.el ends here
