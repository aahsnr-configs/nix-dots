;;; config.el -*- lexical-binding: t; -*-

;; User Information
(setq user-full-name "Ahsanur Rahman"
      user-mail-address "ahsanur041@proton.me")

;;; Core Emacs Settings
(setq-default scroll-conservatively 101
              scroll-margin 0
              scroll-preserve-screen-position t

              ;; Indentation
              indent-tabs-mode nil
              tab-width 2
              fill-column 80

              ;; Line spacing
              line-spacing 0.02)

;; Window splitting preferences
(setq split-width-threshold 170
      split-height-threshold nil)

;; Frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        " - Doom Emacs"))

;;; UI & Theming
(setq doom-theme 'doom-tokyo-night
      doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14.5 :weight 'medium)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 14.5)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 26))

;; Modeline configuration
(after! doom-modeline
  (setq doom-modeline-height 28
        doom-modeline-bar-width 3
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-major-mode-icon t
        doom-modeline-vcs-max-length 12))

;; Which-key
(setq which-key-idle-delay 0.3)

;;; Evil Mode Configuration
(after! evil
  (setq evil-want-fine-undo t
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-move-beyond-eol t))

(after! evil-escape
  (setq evil-escape-key-sequence "jk"
        evil-escape-delay 0.2))

;; Visual line navigation
(map! :nv "j" #'evil-next-visual-line
      :nv "k" #'evil-previous-visual-line)

;;; Completion Framework
(after! corfu
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 2))

(after! vertico
  (setq vertico-count 10))

;;; Project & File Management
(after! projectile
  (setq projectile-project-search-path '("~/projects/" "~/org/")))

;; Dired configuration
(after! dired
  (setq dired-listing-switches "-agho --group-directories-first"
        delete-by-moving-to-trash t
        dired-dwim-target t))

(use-package! dired-open
  :after dired
  :config
  (setq dired-open-extensions '(("png" . "imv")
                                ("mp4" . "mpv"))))

;;; Version Control
(after! magit
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package! magit-todos
  :after magit
  :config (magit-todos-mode 1))

(setq forge-owned-accounts '(("aahsnr")))

;;; Python Development Configuration
;; 1. LSP Configuration with eglot & basedpyright
(after! eglot
  (add-to-list 'eglot-server-programs
               '((python-ts-mode) . ("basedpyright-langserver" "--stdio"))))

;; 2. Code Formatting with apheleia
(after! apheleia
  (setf (alist-get 'isort apheleia-formatters)
        '("isort" "--stdout" "-")
        (alist-get 'ruff apheleia-formatters)
        '("ruff" "format" "--stdin-filename" filepath "-")
        (alist-get 'ruff-isort apheleia-formatters)
        '("sh" "-c" "isort --stdout - | ruff format --stdin-filename " filepath " -"))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff-isort))

;; 3. Syntax Checking with flymake
(after! flymake
  (setq flymake-no-changes-timeout 0.5)
  (defun +python/flymake-ruff ()
    "Flymake backend for ruff."
    (flymake-proc-simple-cleanup)
    (let* ((buffer-file (buffer-file-name))
           (command `("ruff" "check" "--quiet" "--stdin-filename" ,buffer-file "-")))
      (flymake-proc-compile-make-diagnostic-matcher
       command
       :rx (rx line-start (group (+ (not (any ":")))) ":" (+ digit) ":" (+ digit) ": " (group (+ (not (any ":")))) ": " (group (* nonl)))
       :severity (lambda (type) (pcase type ("E" :error) ("W" :warning) (_ :note))))))
  (defun +python/flymake-mypy ()
    "Flymake backend for mypy."
    (when (executable-find "mypy")
      (flymake-proc-simple-cleanup)
      (let ((buffer-file (buffer-file-name)))
        (flymake-proc-compile-make-diagnostic-matcher
         `("mypy" "--show-column-numbers" "--no-error-summary" ,buffer-file)
         :rx (rx line-start (group (+ (not (any ":")))) ":" (+ digit) ":" (+ digit) ": " (group (or "error" "warning" "note")) ": " (group (* nonl)))
         :severity (lambda (type) (pcase type ("error" :error) ("warning" :warning) (_ :note)))))))
  (add-hook! 'python-ts-mode-hook
    (defun +python/setup-flymake-h ()
      (add-hook 'flymake-diagnostic-functions #'+python/flymake-ruff nil t)
      (add-hook 'flymake-diagnostic-functions #'+python/flymake-mypy nil t))))

;; 4. Debugging with dape & debugpy
(after! dape
  (add-to-list 'dape-configs
               `(debugpy modes (python-ts-mode) command "python" command-args ("-m" "debugpy.adapter")
                 :type "executable" :request "launch" :cwd dape-cwd-fn :program dape-find-file-buffer-default))
  (add-to-list 'dape-configs
               `(debugpy-module modes (python-ts-mode) command "python" command-args ("-m" "debugpy.adapter")
                 :type "executable" :request "launch" :module (read-string "Module: ") :cwd dape-cwd-fn))
  (add-to-list 'dape-configs
               `(debugpy-attach modes (python-ts-mode) command "python" command-args ("-m" "debugpy.adapter")
                 :type "executable" :request "attach" :connect (:host "localhost" :port (read-number "Port: " 5678))
                 :pathMappings [(:localRoot dape-cwd-fn :remoteRoot dape-cwd-fn)])))

(add-hook! 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode)

;;; Jupyter Configuration
(after! org
  (setq org-babel-jupyter-override-src-block "python"
        jupyter-repl-echo-eval-p nil))

;;; Org Mode Configuration
(defvar my/org-directory "~/org/" "The root directory for Org files.")
(defvar my/org-roam-directory (expand-file-name "roam/" my/org-directory) "The directory for Org Roam files.")

(after! org
  (setq org-directory my/org-directory
        org-agenda-files (list (expand-file-name "inbox.org" my/org-directory)
                               (expand-file-name "projects.org" my/org-directory)
                               (expand-file-name "habits.org" my/org-directory))
        org-default-notes-file (expand-file-name "inbox.org" my/org-directory)
        org-src-fontify-natively t
        org-src-window-setup 'current-window
        org-confirm-babel-evaluate nil
        org-startup-with-inline-images t
        org-image-actual-width 600
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-archive-location (concat my/org-directory "archive/%s_archive::")
        org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCEL(c@)")
          (sequence "PLAN(P)" "ACTIVE(A)" "PAUSED(x)" "|" "ACHIEVED(a)" "DROPPED(D)"))))

;; Org-roam configuration
(after! org-roam
  (setq org-roam-directory my/org-roam-directory
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-completion-everywhere t))

(use-package! org-roam-ui
  :after org-roam
  :config (setq org-roam-ui-sync-theme t
                org-roam-ui-follow t
                org-roam-ui-update-on-save t))

(use-package! consult-org-roam
  :after org-roam
  :init (consult-org-roam-mode 1))

(use-package! org-super-agenda
  :after org-agenda
  :config (org-super-agenda-mode))

(add-hook! 'org-mode-hook #'org-fragtog-mode)

(after! org-appear
  (setq org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autosubmarkers t))

;;; LaTeX Configuration
(after! latex
  (setq TeX-engine 'xetex
        TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-mode t))

(add-hook! 'LaTeX-mode-hook #'laas-mode)

;; Citations
(use-package! citar-org-roam
  :after (citar org-roam)
  :config (citar-org-roam-mode))

;;; Snippets
(use-package! yasnippet-capf
  :after cape
  :config (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;;; PDF Tools
(setq-default pdf-view-display-size 'fit-page)
(add-hook! 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)

(setq-default vterm-shell "/usr/bin/fish"
              explicit-shell-file-name "/usr/bin/fish")

(setq confirm-kill-emacs nil)

;;; Keybindings
(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Toggle eshell split"            "e" #'+eshell/toggle
       :desc "Toggle line highlight in frame" "h" #'hl-line-mode
       :desc "Toggle line highlight globally" "H" #'global-hl-line-mode
       :desc "Toggle line numbers"            "l" #'doom/toggle-line-numbers
       :desc "Toggle truncate lines"          "t" #'toggle-truncate-lines
       :desc "Toggle treemacs"                "T" #'+treemacs/toggle
       :desc "Toggle vterm split"             "v" #'+vterm/toggle))

(map! :leader
      (:prefix ("o" . "open here")
       :desc "Open eshell here"    "e" #'+eshell/here
       :desc "Open vterm here"     "v" #'+vterm/here))


(map! :leader
      :desc "M-x" "SPC" #'execute-extended-command)

;; Literate Programming
(map! :leader
      (:prefix ("l" . "literate")
       :desc "Tangle file" "t" #'org-babel-tangle
       :desc "Execute buffer" "x" #'org-babel-execute-buffer))

;; Python debugging
;; (map! :leader
;;       (:prefix ("d" . "debug/dape")
;;        :desc "Debug" "d" #'dape
;;        :desc "Toggle breakpoint" "b" #'dape-breakpoint-toggle
;;        :desc "Continue" "c" #'dape-continue
;;        :desc "Next" "n" #'dape-next
;;        :desc "Step in" "i" #'dape-step-in
;;        :desc "Step out" "o" #'dape-step-out
;;        :desc "Restart" "r" #'dape-restart
;;        :desc "Kill debug session" "k" #'dape-kill
;;        :desc "Debug REPL" "R" #'dape-repl))

;; ;; Code actions
;; (map! :leader
;;       :prefix ("c" . "code")
;;       :desc "Execute code action" "a" #'eglot-code-actions
;;       :desc "Rename" "r" #'eglot-rename
;;       :desc "Format buffer" "f" #'apheleia-format-buffer)

;; ;; Terminal
;; (map! :leader
;;       :prefix ("o" . "open")
;;       :desc "Toggle vterm" "t" #'+vterm/toggle
;;       :desc "Vterm here" "T" #'+vterm/here)

;; ;; Python-specific keybindings
;; (map! :localleader
;;       :map python-ts-mode-map
;;       "r" #'python-shell-send-region
;;       "b" #'python-shell-send-buffer
;;       "f" #'python-shell-send-file
;;       "i" #'run-python
;;       :prefix ("j" . "jupyter")
;;       :desc "Run cell" "c" #'jupyter-eval-buffer
;;       :desc "Run line/region" "l" #'jupyter-eval-line-or-region)

(use-package! rainbow-delimiters
  :hook ((text-mode . rainbow-delimiters-mode)
         (prog-mode . rainbow-delimiters-mode)
         (org-src-mode-hook . rainbow-delimiters-mode))

  ;; Custom faces updated for the Tokyonight color palette.
  :custom-face
  (rainbow-delimiters-depth-1-face ((t (:foreground "#7aa2f7"))))  ; Blue
  (rainbow-delimiters-depth-2-face ((t (:foreground "#bb9af7"))))  ; Magenta
  (rainbow-delimiters-depth-3-face ((t (:foreground "#e0af68"))))  ; Yellow
  (rainbow-delimiters-depth-4-face ((t (:foreground "#73daca"))))  ; Cyan
  (rainbow-delimiters-depth-5-face ((t (:foreground "#f7768e"))))  ; Red
  (rainbow-delimiters-depth-6-face ((t (:foreground "#9ece6a"))))  ; Green
  (rainbow-delimiters-depth-7-face ((t (:foreground "#ff9e64"))))  ; Orange
  (rainbow-delimiters-depth-8-face ((t (:foreground "#c0caf5"))))  ; Foreground
  (rainbow-delimiters-depth-9-face ((t (:foreground "#a9b1d6"))))) ; Sub-Foreground
