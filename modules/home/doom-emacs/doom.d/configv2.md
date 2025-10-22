Of course! Here is the complete, combined `config.org` file presented in a single, properly formatted Markdown output, with the summary of changes included as requested.

---

Based on the information provided, here is the complete, properly formatted `config.org` with Doom Emacs syntax and clear change markers.

```org
#+title: My Doom Emacs Config
#+author: Ahsanur Rahman

This is my personal Doom Emacs configuration, organized in a literate programming style using Org mode. This file is the single source of truth; the actual `config.el` is generated ("tangled") from the source blocks herein.

* Personal Information
This section sets my personal details, which are used by various Emacs packages (e.g., for sending emails or creating file headers).

#+begin_src emacs-lisp
;; User Information
(setq user-full-name "Ahsanur Rahman"
      user-mail-address "ahsanur041@proton.me")
#+end_src

* Core Emacs Settings
These are fundamental settings that control the basic behavior of Emacs, such as scrolling, indentation, and window management. They are set first to establish a consistent foundation for the rest of the configuration.

** Scrolling and Display
These settings provide a smoother and more predictable scrolling experience.
#+begin_src emacs-lisp
(setq-default scroll-conservatively 101
              scroll-margin 0
              scroll-preserve-screen-position t)
#+end_src

** Indentation and Text
We enforce spaces over tabs and a standard line width of 80 characters. A small amount of line spacing is added for readability.
#+begin_src emacs-lisp
(setq-default ;; Indentation
              indent-tabs-mode nil
              tab-width 2
              fill-column 80

              ;; Line spacing
              line-spacing 0.02)
#+end_src

** Window Management
This configuration dictates how Emacs splits windows, preferring vertical splits for wider screens.
#+begin_src emacs-lisp
(setq split-width-threshold 170
      split-height-threshold nil)
#+end_src

** Frame Title
This customizes the title bar of the Emacs window to show the current buffer name and system name for easy identification.
#+begin_src emacs-lisp
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        " - Doom Emacs"))
#+end_src

** Custom Doom Emacs Directory
The following piece of configuration allows me to directorly go to the doom configuration files in my nixos configurations
#+begin_src emacs-lisp
;; Override doom-private-dir for NixOS with nix-doom-emacs-unstraightened
;; This makes SPC f p and related commands go to the editable config
(setq doom-private-dir "~/nix-dots/modules/home/doom-emacs/doom.d/")

;; Override the default/find-in-config function to use our custom directory
(defun my/find-in-doom-config ()
  "Browse files in your editable Doom config directory."
  (interactive)
  (doom-project-browse doom-private-dir))

;; Bind SPC f p to our custom function
(map! :leader
      :desc "Find file in private config" "f p" #'my/find-in-doom-config)

;; Also override the individual config file shortcuts
(map! :leader
      :prefix "f"
      :desc "Open doom config.org" "P c"
      (lambda () (interactive)
        (find-file (expand-file-name "config.org" doom-private-dir)))
      :desc "Open doom init.el" "P i"
      (lambda () (interactive)
        (find-file (expand-file-name "init.el" doom-private-dir)))
      :desc "Open doom packages.el" "P p"
      (lambda () (interactive)
        (find-file (expand-file-name "packages.el" doom-private-dir))))
#+end_src

* UI & Theming
This section covers the visual appearance of Emacs, including themes, fonts, the modeline, and other aesthetic elements.

** Fonts
#+begin_src emacs-lisp
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14.5 :weight 'medium)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 14.5)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 26))
#+end_src

** Catppuccin Theme Customization
Override specific colors for better contrast and visual improvements, with special attention to background shades for different UI elements.
#+begin_src emacs-lisp
(use-package! catppuccin-theme
  :config
  (setq catppuccin-flavor 'mocha)
  (setq doom-theme 'catppuccin)

  ;; Enable Catppuccin quality-of-life features
  (setq catppuccin-italic-comments t
        catppuccin-italic-blockquotes t
        catppuccin-italic-variables nil
        catppuccin-highlight-matches t
        catppuccin-dark-line-numbers-background t)

  ;; Apply custom faces for a unique and consistent color scheme.
  ;; The hexadecimal colors are from the Catppuccin Mocha palette:
  ;; #11111b (crust), #181825 (mantle), #1e1e2e (base), #24273a (surface0)
  (custom-theme-set-faces! 'catppuccin
    ;; 1. Default background is a lighter shade (surface0)
    '(default :background "#1e1e2e" :foreground "#cdd6f4")

    ;; 2. Popup completion system is a darker shade (base)
    '(corfu-default :background "#1e1e2e" :foreground "#cdd6f4")

    ;; 3. Solaire-mode has the darkest accent color (crust)
    '(solaire-mode-bg-face :background "#11111b")

    ;; 4. Highlighted line is a unique, lighter shade than solaire (mantle)
    '(hl-line :background "#11111b" :extend t)

    ;; --- Other UI Enhancements ---
    ;; Org mode improvements
    '(org-block :background "#313244" :foreground "#cdd6f4" :extend t)
    '(org-block-begin-line :background "#313244" :foreground "#6c7086" :extend t)
    '(org-block-end-line :background "#313244" :foreground "#6c7086" :extend t)
    '(org-meta-line :foreground "#6c7086")
    '(org-document-info-keyword :foreground "#6c7086")

    ;; Enhanced modeline contrast
    '(mode-line :background "#181825" :foreground "#cdd6f4")
    '(mode-line-inactive :background "#11111b" :foreground "#6c7086")

    ;; Better selection visibility
    '(region :background "#585b70" :extend t)

    ;; Improved cursor visibility
    '(cursor :background "#f5e0dc")

    ;; Enhanced matching parens
    '(show-paren-match :foreground "#f5c2e7" :background "#45475a" :weight bold)

    ;; Better minibuffer
    '(minibuffer-prompt :foreground "#89dceb" :weight bold)))
#end_src

** Frame Padding
A small internal border is added around the frame to create visual breathing room between the text and the window edge.
#+begin_src emacs-lisp
(setq-default internal-border-width 5)
(add-to-list 'default-frame-alist '(internal-border-width . 5))
#+end_src

** Modeline
The modeline is the information bar at the bottom of each window. We use `doom-modeline` and customize its appearance for better readability and information density.
#+begin_src emacs-lisp
(after! doom-modeline
  (setq doom-modeline-height 28
        doom-modeline-bar-width 3
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-major-mode-icon t
        doom-modeline-vcs-max-length 12
        doom-modeline-icon t
        doom-modeline-modal t
        doom-modeline-modal-icon t
        doom-modeline-buffer-encoding nil
        doom-modeline-indent-info nil
        doom-modeline-enable-word-count nil))
#+end_src

** Line Numbers
Line numbers are enabled globally by default, which is useful for programming. We then selectively disable them in modes where they are distracting, such as Org, Dired, and Magit.
#+begin_src emacs-lisp
;; Enable absolute line numbers globally by default.
(setq display-line-numbers-type t)

;; Disable line numbers in modes where they aren't useful.
(add-hook! '(org-mode-hook
             dired-mode-hook
             magit-status-mode-hook
             eshell-mode-hook
             vterm-mode-hook
             help-mode-hook
             doom-dashboard-mode-hook)
           #'(lambda () (display-line-numbers-mode -1)))
#+end_src

** Which-key
`which-key` displays available keybindings after a prefix key is pressed. We configure a short delay for responsiveness.
#+begin_src emacs-lisp
(setq which-key-idle-delay 0.3
      which-key-allow-imprecise-window-fit nil)
#+end_src

** Rainbow Delimiters
Catppuccin theme provides rainbow-delimiters colors using its native palette for perfect color harmony.
#+begin_src emacs-lisp
(use-package! rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)
         (org-src-mode . rainbow-delimiters-mode)
         (treesit-auto-mode-hook . rainbow-delimiters-mode))

  ;; Catppuccin Mocha palette - these are already defined by the theme
  ;; but we customize for optimal nesting visibility
  :custom-face
  (rainbow-delimiters-depth-1-face ((t (:foreground "#f38ba8"))))  ; Red
  (rainbow-delimiters-depth-2-face ((t (:foreground "#fab387"))))  ; Peach
  (rainbow-delimiters-depth-3-face ((t (:foreground "#f9e2af"))))  ; Yellow
  (rainbow-delimiters-depth-4-face ((t (:foreground "#a6e3a1"))))  ; Green
  (rainbow-delimiters-depth-5-face ((t (:foreground "#74c7ec"))))  ; Sapphire
  (rainbow-delimiters-depth-6-face ((t (:foreground "#b4befe"))))  ; Lavender
  (rainbow-delimiters-depth-7-face ((t (:foreground "#cba6f7"))))  ; Mauve
  (rainbow-delimiters-unmatched-face ((t (:foreground "#f38ba8" :weight bold)))))
#+end_src

** Markdown Styling
We customize the faces for Markdown headers to make them larger and more distinct. A helper function is also defined to easily toggle between the raw Markdown and a rendered preview.
#+begin_src emacs-lisp
;; Headers with Catppuccin colors
(custom-set-faces
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "variable-pitch"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.6 :foreground "#f38ba8"))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.5 :foreground "#fab387"))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.4 :foreground "#f9e2af"))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.3 :foreground "#a6e3a1"))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.2 :foreground "#74c7ec"))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 1.1 :foreground "#b4befe")))))

;; Toggle Markdown View
(defun dt/toggle-markdown-view-mode ()
  "Toggle between `markdown-mode' and `markdown-view-mode'."
  (interactive)
  (if (eq major-mode 'markdown-view-mode)
      (markdown-mode)
    (markdown-view-mode)))
#+end_src

* Evil Mode
This section configures `evil-mode`, the Vim emulation layer that provides modal editing capabilities within Emacs.

#+begin_src emacs-lisp
(after! evil
  (setq evil-want-fine-undo t
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-move-beyond-eol t))

(after! evil-escape
  (setq evil-escape-key-sequence "jk"
        evil-escape-delay 0.2))

;; Use visual line navigation, which is more intuitive when working with wrapped lines.
(map! :nv "j" #'evil-next-visual-line
      :nv "k" #'evil-previous-visual-line)
#+end_src

* Completion Framework
This configures the packages responsible for in-buffer completion (`corfu`) and minibuffer completion (`vertico`), creating a modern and powerful interactive experience.

** Corfu
`corfu` provides a clean, pop-up completion UI for text being typed directly in a buffer.
#+begin_src emacs-lisp
(after! corfu
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 2))
#+end_src

** Vertico
`vertico` provides a vertical, interactive list for minibuffer commands like `find-file` and `M-x`.
#+begin_src emacs-lisp
(after! vertico
  (setq vertico-count 10))
#+end_src

* Project & File Management
Settings related to managing projects and navigating the file system.

** Projectile
`projectile` is a project interaction library for Emacs. We tell it where to look for our projects.
#+begin_src emacs-lisp
(after! projectile
  (setq projectile-project-search-path '("~/projects/" "~/org/")))
#+end_src

** Dired (Directory Editor)
Configuration for Emacs's built-in file manager, `dired`. We set custom listing switches and configure it to use the system trash. We also use `dired-open` to specify external applications for certain file types.
#+begin_src emacs-lisp
(after! dired
  (setq dired-listing-switches "-agho --group-directories-first"
        delete-by-moving-to-trash t
        dired-dwim-target t))

(use-package! dired-open
  :after dired
  :config
  (setq dired-open-extensions '(("png" . "imv")
                                ("mp4" . "mpv"))))
#+end_src

* Version Control (Magit)
Configuration for `magit`, the powerful Git client inside Emacs.

#+begin_src emacs-lisp
(after! magit
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package! magit-todos
  :after magit
  :config (magit-todos-mode 1))

(setq forge-owned-accounts '(("aahsnr")))
#+end_src

* Programming
This section contains all configurations related to software development, including language-specific setups for Python and LaTeX.

** General Tools
These are tools that apply to most programming languages, such as the LSP client, debugger, and syntax checkers.

*** Flymake Collection
`flymake-collection` provides a convenient way to set up syntax checkers (`linters`) for various languages without extensive manual configuration.
#+begin_src emacs-lisp
(use-package! flymake-collection
  :after flymake
  :config
  (flymake-collection-hook-setup))
#+end_src

*** Eglot (LSP Client)
;; CHANGED: Fixed eldoc-box setup - using hover-mode instead of hover-at-point-mode for better performance
We enable `eldoc-box` to show documentation in a pop-up box whenever an LSP server is active.
#+begin_src emacs-lisp
(add-hook! 'eglot-managed-mode-hook #'eldoc-box-hover-mode)
#+end_src

*** Snippets (YASnippet)
We use `yasnippet-capf` to integrate snippet expansion with the Corfu completion framework.
#+begin_src emacs-lisp
(use-package! yasnippet-capf
  :after cape
  :config (add-to-list 'completion-at-point-functions #'yasnippet-capf))
#+end_src

** Python
This provides a complete Python development environment using Tree-sitter for syntax highlighting, *basedpyright* for LSP features, *ruff* for formatting and linting, and *debugpy* for debugging.

*** LSP Configuration
We tell *eglot* to use the *basedpyright-langserver* for Python files that are in *python-ts-mode*.
#+begin_src emacs-lisp
(after! eglot
  (add-to-list 'eglot-server-programs
               '((python-ts-mode) . ("basedpyright-langserver" "--stdio"))))
#+end_src

*** Code Formatting (Apheleia)
We configure *apheleia* to use a combination of *isort* and *ruff* to automatically format Python code on save.
#+begin_src emacs-lisp
(after! apheleia
  (setf (alist-get 'ruff apheleia-formatters)
        '("ruff" "format" "--stdin-filename" filepath "-"))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff))
#+end_src

*** Syntax Checking (Flymake)
Using `flymake-collection`, we enable `ruff` as a high-performance linter and `mypy` for static type checking.
#+begin_src emacs-lisp
(after! python-ts-mode
  (setq-default flymake-collection-python-pylint-executable "ruff")
  (setq-default flymake-collection-python-mypy-executable "mypy")
  (flymake-collection-add-for-major-mode
   'python-ts-mode
   'flymake-collection-python-ruff ; This will use ruff as defined above
   'flymake-collection-python-mypy))
#+end_src

*** Debugging (DAPE)
We configure `dape` to use `debugpy` as the debug adapter for Python, enabling full debugging capabilities inside Emacs.
#+begin_src emacs-lisp
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
#+end_src

** LaTeX
Configuration for writing LaTeX documents, including PDF viewing and citation management.
#+begin_src emacs-lisp
(after! latex
  (setq TeX-engine 'xetex
        TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-mode t))

(add-hook! 'LaTeX-mode-hook #'laas-mode)

;; Citar integration with Org Roam for managing literature notes.
(use-package! citar-org-roam
  :after (citar org-roam)
  :config (citar-org-roam-mode))
#+end_src

* Org Mode
This is the central hub for my personal knowledge management, task tracking, and literate programming.

** Core Setup
We define custom directories and set foundational Org mode behaviors. This includes defining agenda files, enabling native fontification of source blocks, and setting custom TODO keywords. We also add a hook to enable parenthesis highlighting in source edit buffers.
#+begin_src emacs-lisp
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
#+end_src

** Org Babel & Jupyter
Configuration for executing code blocks and Jupyter integration.
#+begin_src emacs-lisp
;; CRITICAL: Remove Doom's advice that re-enables jupyter-org-interaction-mode
(after! ob-jupyter
  (advice-remove 'org-babel-jupyter-initiate-session
                 #'+org--ob-jupyter-initiate-session-a)

  ;; Set default header arguments for jupyter-python
  (setq org-babel-default-header-args:jupyter-python
        '((:async . "yes")
          (:session . "py")
          (:kernel . "python3"))))

;; Remove jupyter completion from org-mode
(after! jupyter-org-client
  ;; Actively remove jupyter completion function
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (delq 'jupyter-org-completion-at-point
                                completion-at-point-functions)))
            90))

;; Disable undo-fu-session for org files with jupyter blocks
(after! undo-fu-session
  (add-to-list 'undo-fu-session-incompatible-major-modes 'org-mode))
#+end_src

** Org-Src Edit Buffer Integration
;; NEW SECTION: Enable Eglot LSP support in org-edit-special buffers for Python/Jupyter blocks
This configuration enables full LSP support when editing Python source blocks in org-mode using `org-edit-special` (C-c ').
It requires `:tangle` headers on your source blocks to determine the project context.

#+begin_src emacs-lisp
;; Ensure jupyter-python blocks use python-ts-mode in edit buffers
(after! org-src
  (add-to-list 'org-src-lang-modes '("jupyter-python" . python-ts)))

;; Use org-babel-edit-prep to configure Eglot before entering edit buffer
;; This function is called automatically by org-mode when you use C-c ' to edit a source block
(defun org-babel-edit-prep:python (babel-info)
  "Prepare Python src block for editing with Eglot.
Sets buffer-file-name and default-directory so Eglot can find the project root.
Requires a :tangle header in the source block."
  (let* ((params (nth 2 babel-info))
         (tangle-file (assoc-default :tangle params)))
    (when (and tangle-file
               (not (string= tangle-file "no"))
               (not (string= tangle-file "nil")))
      ;; Set buffer-file-name to the tangle target for project detection
      (setq-local buffer-file-name (expand-file-name tangle-file))
      ;; Ensure we're in the right directory for project detection
      (setq-local default-directory (file-name-directory buffer-file-name))
      ;; Start Eglot (which will automatically enable eldoc-box-hover-mode)
      (eglot-ensure))))

;; Also handle jupyter-python blocks specifically
(defun org-babel-edit-prep:jupyter-python (babel-info)
  "Prepare Jupyter Python src block for editing with Eglot."
  (org-babel-edit-prep:python babel-info))

;; Fix C-c C-c keybinding in org-src-edit buffers
;; By default, jupyter-mode binds C-c C-c to eval, but we want it to exit the edit buffer
(defun my/org-src-fix-keybindings ()
  "Override C-c C-c in org-src-mode to exit the edit buffer cleanly."
  (when (and (bound-and-p 'org-src-mode) org-src-mode)
    (local-set-key (kbd "C-c C-c") #'org-edit-src-exit)))

(add-hook! 'org-src-mode-hook #'my/org-src-fix-keybindings)
#+end_src

** Org Roam
`org-roam` is a powerful note-taking tool for building a personal knowledge graph, inspired by the Zettelkasten method. We also enable `org-roam-ui` for a visual graph interface.
#+begin_src emacs-lisp
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
#+end_src

** UI Enhancements
These packages improve the visual presentation of Org mode. *org-super-agenda* provides powerful grouping for agenda views, while *org-fragtog* and *org-appear* enhance the display of LaTeX fragments and emphasis markers.
#+begin_src emacs-lisp
(use-package! org-super-agenda
  :after org-agenda
  :hook (org-agenda-mode-hook . org-super-agenda-mode))

(add-hook! 'org-mode-hook #'org-fragtog-mode)

(after! org-appear
  (setq org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autosubmarkers t))

(after! org
  ;; Set custom faces for scaled org headers to improve visual hierarchy.
  (custom-set-faces!
    '(org-level-1 :inherit 'variable-pitch :weight bold :height 1.2)
    '(org-level-2 :inherit 'variable-pitch :weight bold :height 1.13)
    '(org-level-3 :inherit 'variable-pitch :weight bold :height 1.10)
    '(org-level-4 :inherit 'variable-pitch :weight bold :height 1.07)
    '(org-level-5 :inherit 'variable-pitch :weight bold :height 1.05)
    '(org-level-6 :inherit 'variable-pitch :weight bold :height 1.03)
    '(org-level-7 :inherit 'variable-pitch :weight bold :height 1.02)
    '(org-level-8 :inherit 'variable-pitch :weight bold :height 1.0)))
#+end_src

** Org Modern
#+begin_src emacs-lisp
(after! org-modern
  (setq
   ;; Override Doom's dynamic star visibility with a consistent character.
   org-modern-hide-stars "· "
   ;; Customize the appearance of headline stars/bullets.
   org-modern-star '("◉" "○" "◈" "◇" "◆" "▷")
   ;; Customize list item bullets.
   org-modern-list '((43 . "➤") (45 . "–") (42 . "•"))
   ;; Adjust table line appearance.
   org-modern-table-vertical 1
   org-modern-table-horizontal 0.1
   ;; Customize the block name delimiters.
   org-modern-block-name '(("src" "»" "«")
                           ("example" "»" "«")
                           ("quote" """ """))
   ;; Define custom checkbox characters.
   org-modern-checkbox '((todo . "☐") (done . "☑") (cancel . "☒") (priority . "⚑") (on . "◉") (off . "◯"))
   ;; Override Doom's derived tag faces with a specific style for Catppuccin.
   org-modern-tag-faces `((:foreground ,(face-attribute 'default :foreground) :weight bold :box (:line-width (1 . -1) :color "#45475a")))))
#+end_src

* Keybindings
This section defines my custom keybindings, primarily using the leader key (`SPC`).

** General Toggles and Actions
#+begin_src emacs-lisp
(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Toggle eshell split"            "e" #'+eshell/toggle
       :desc "Toggle line highlight in frame" "h" #'hl-line-mode
       :desc "Toggle line highlight globally" "H" #'global-hl-line-mode
       :desc "Toggle line numbers"            "l" #'doom/toggle-line-numbers
       :desc "Toggle markdown-view-mode"      "m" #'dt/toggle-markdown-view-mode
       :desc "Toggle truncate lines"          "t" #'toggle-truncate-lines
       :desc "Toggle treemacs"                "T" #'+treemacs/toggle
       :desc "Toggle vterm split"             "v" #'+vterm/toggle))

(map! :leader
      (:prefix ("o" . "open here")
       :desc "Open eshell here"    "e" #'+eshell/here
       :desc "Open vterm here"     "v" #'+vterm/here))

(map! :leader
      :desc "M-x" "SPC" #'execute-extended-command)
#+end_src

** Literate Programming (Org Babel)
#+begin_src emacs-lisp
(map! :leader
      (:prefix ("l" . "literate")
       :desc "Tangle file" "t" #'org-babel-tangle
       :desc "Execute buffer" "x" #'org-babel-execute-buffer))
#+end_src

** Debugging (DAPE)
Global keybindings for the Debug Adapter Protocol client.
#+begin_src emacs-lisp
(map! :leader
      (:prefix ("d" . "debug/dape")
       :desc "Debug" "d" #'dape
       :desc "Toggle breakpoint" "b" #'dape-breakpoint-toggle
       :desc "Continue" "c" #'dape-continue
       :desc "Next" "n" #'dape-next
       :desc "Step in" "i" #'dape-step-in
       :desc "Step out" "o" #'dape-step-out
       :desc "Restart" "r" #'dape-restart
       :desc "Kill debug session" "k" #'dape-kill
       :desc "Debug REPL" "R" #'dape-repl))
#+end_src

* Miscellaneous
A place for settings that don't fit neatly into the other categories.

** PDF Tools
Default settings for viewing PDF files inside Emacs.
#+begin_src emacs-lisp
(setq-default pdf-view-display-size 'fit-page)
(add-hook! 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)
#+end_src

** Default Shell
We set `fish` as the default shell for terminal emulators like `vterm`.
#+begin_src emacs-lisp
(setq-default vterm-shell "/usr/bin/fish"
              explicit-shell-file-name "/usr/bin/fish")
#+end_src

** Quit Confirmation
Disable the "Are you sure you want to quit Emacs?" prompt.
#+begin_src emacs-lisp
(setq confirm-kill-emacs nil)
#+end_src

** Suppress Warnings
Disable annoying warnings that don't affect functionality.
#+begin_src emacs-lisp
;; Suppress org-element warnings in non-org buffers
(setq warning-suppress-types '((org-element)))
#+end_src
```

## Summary of Changes

### 1. **Eglot (LSP Client) Section**

**CHANGED:** Replaced `eldoc-box-hover-at-point-mode` with `eldoc-box-hover-mode` for better performance and less intrusive documentation display.

### 2. **New Section: Org-Src Edit Buffer Integration** (After "Org Babel & Jupyter")

**ADDED:** A complete new section that enables:

- Eglot LSP support in `org-edit-special` buffers.
- Proper project detection via `:tangle` headers.
- Fixed `C-c C-c` keybinding to exit edit buffers correctly.
- Support for both `python` and `jupyter-python` blocks.

### Key Requirements for Usage

To use the new org-src-edit LSP integration, add `:tangle` headers to your source blocks. The tangle file path should be within your project directory (e.g., a `.git` repository) so Eglot can properly detect the project root.

```org
#+begin_src jupyter-python :session py :tangle ~/projects/myproject/analysis.py
import numpy as np
x = np.array([1, 2, 3])
#+end_src
```
