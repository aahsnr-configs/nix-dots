# Emacs Configuration Analysis and Improvements

## Response to First Prompt

---

## 1. Treesit Syntax Highlighting in Org Source Blocks

### Analysis

Tree-sitter can indeed be used for syntax highlighting in org source blocks.
When properly configured, tree-sitter will automatically fontify code blocks
when treesit modes are available.

### Issue with Current Configuration

Setting `(org-src-fontify-natively nil)` disables native fontification entirely,
which prevents both traditional font-lock and tree-sitter from working in source
blocks.

### Solution

**Keep `org-src-fontify-natively t`** - This allows tree-sitter to work
automatically when treesit modes are available. Since you have lsp-bridge active
in org source blocks, syntax highlighting will work through the combination of:

1. Native org fontification (enabled)
2. Tree-sitter modes (auto-enabled when available)
3. LSP-bridge semantic tokens

### Recommended Configuration Change

```elisp
;; In Core Configuration section - Org Mode
(org-src-fontify-natively t)  ; Keep this enabled for tree-sitter
(org-fontify-quote-and-verse-blocks nil)
(org-fontify-whole-heading-line nil)
(org-element-use-cache t)
(org-element-cache-persistent t)
```

---

## 2. Missing Packages for Default Vim-like Behavior

### Currently Missing Features

Your configuration is mostly complete, but these additions would enhance
vim-like behavior:

```elisp
;; Add to Evil Extensions section
(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :ensure t
  :after evil
  :config
  (global-evil-matchit-mode 1))

;; Enhanced number increment/decrement (you already have evil-numbers)
(with-eval-after-load 'evil-numbers
  (define-key evil-normal-state-map (kbd "C-a") #'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-x") #'evil-numbers/dec-at-pt)
  (define-key evil-visual-state-map (kbd "g C-a") #'evil-numbers/inc-at-pt-incremental)
  (define-key evil-visual-state-map (kbd "g C-x") #'evil-numbers/dec-at-pt-incremental))
```

### Update default.nix

```nix
extraEmacsPackages = epkgs:
  with epkgs; [
    use-package
    vterm
    jupyter
    visual-fill-column
    auto-dim-other-buffers
    treesit-grammars.with-all-grammars
    evil-surround  # Add this
    evil-matchit   # Add this
  ];
```

---

## 3. Doom Emacs Evil Implementation

### Key Doom Evil Features

Based on analysis of Doom's evil configuration, here are the essential
improvements:

### Basic Evil Enhancements

```elisp
;; Replace existing evil configuration's :custom section
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)

  :config
  (evil-mode 1)

  :custom
  (evil-undo-system 'undo-fu)
  (evil-ex-visual-char-range t)
  (evil-ex-search-vim-style-regexp t)
  (evil-echo-state nil)
  (evil-move-cursor-back nil)
  (evil-v$-excludes-newline t)
  (evil-want-C-h-delete t)
  (evil-want-C-u-delete t)
  (evil-want-fine-undo t)
  (evil-move-beyond-eol t)
  (evil-search-wrap t)
  (evil-symbol-word-search t))
```

### Doom-style o/O Behavior (Comment Continuation)

```elisp
;; Add to Evil Extensions section
(defcustom +evil-want-o/O-to-continue-comments t
  "If non-nil, o/O keys will continue comment lines."
  :type 'boolean
  :group 'evil)

(defun +evil/insert-newline-below (count)
  "Insert COUNT newlines below, respecting comments."
  (interactive "p")
  (if (and (fboundp 'sp-point-in-comment)
           (sp-point-in-comment)
           +evil-want-o/O-to-continue-comments)
      (let ((comment-start (or comment-start ""))
            (comment-padding (or comment-padding " ")))
        (end-of-line)
        (newline-and-indent)
        (when (and comment-start (not (string-empty-p comment-start)))
          (insert comment-start comment-padding)))
    (evil-open-below count))
  (evil-insert-state))

(defun +evil/insert-newline-above (count)
  "Insert COUNT newlines above, respecting comments."
  (interactive "p")
  (if (and (fboundp 'sp-point-in-comment)
           (sp-point-in-comment)
           +evil-want-o/O-to-continue-comments)
      (let ((comment-start (or comment-start ""))
            (comment-padding (or comment-padding " ")))
        (beginning-of-line)
        (newline)
        (forward-line -1)
        (indent-according-to-mode)
        (when (and comment-start (not (string-empty-p comment-start)))
          (insert comment-start comment-padding)))
    (evil-open-above count))
  (evil-insert-state))

;; Override o/O when enabled
(define-key evil-normal-state-map "o" #'+evil/insert-newline-below)
(define-key evil-normal-state-map "O" #'+evil/insert-newline-above)
```

### Doom-style Visual Search (* and # in visual mode)

```elisp
(defun +evil/visual-search-forward ()
  "Search forward for the visual selection."
  (interactive)
  (let* ((beg (region-beginning))
         (end (region-end))
         (selection (buffer-substring-no-properties beg end)))
    (deactivate-mark)
    (evil-search-forward nil nil nil selection)))

(defun +evil/visual-search-backward ()
  "Search backward for the visual selection."
  (interactive)
  (let* ((beg (region-beginning))
         (end (region-end))
         (selection (buffer-substring-no-properties beg end)))
    (deactivate-mark)
    (evil-search-backward nil nil nil selection)))

(define-key evil-visual-state-map "*" #'+evil/visual-search-forward)
(define-key evil-visual-state-map "#" #'+evil/visual-search-backward)
```

### Doom's Improved :global Command

```elisp
(with-eval-after-load 'evil-ex
  (evil-ex-define-cmd "g[lobal]" #'evil-ex-global)
  
  (defun +evil--highlight-global-matches ()
    "Highlight matches for :global command."
    (when-let ((pattern (car evil-ex-global-match)))
      (hi-lock-face-buffer pattern 'hi-yellow)))
  
  (add-hook 'evil-ex-global-hook #'+evil--highlight-global-matches))
```

---

## 4. Doom Emacs Word Wrap Setup

### Implementation

Doom's word-wrap provides intelligent soft-wrapping with language-aware
indentation.

```elisp
;; Custom variables
(defcustom +word-wrap-extra-indent 'double
  "Amount of extra indentation for wrapped code lines.
Values: 'double (2x indent), 'single (1x indent), or nil (no extra indent)."
  :type '(choice (const :tag "Double" double)
                 (const :tag "Single" single)
                 (const :tag "None" nil))
  :group 'word-wrap)

(defcustom +word-wrap-fill-style nil
  "How to wrap long lines.
- nil: wrap at window edge
- 'soft: wrap at fill-column using visual-fill-column
- 'auto: use auto-fill-mode for new lines, soft-wrap existing"
  :type '(choice (const :tag "Window edge" nil)
                 (const :tag "Fill column (soft)" soft)
                 (const :tag "Auto (mixed)" auto))
  :group 'word-wrap)

(defcustom +word-wrap-disabled-modes
  '(fundamental-mode so-long-mode special-mode)
  "Major modes where word-wrap should not be enabled."
  :type '(repeat symbol)
  :group 'word-wrap)

(defcustom +word-wrap-text-modes
  '(text-mode markdown-mode gfm-mode org-mode)
  "Major modes that are primarily text (no extra indent)."
  :type '(repeat symbol)
  :group 'word-wrap)

;; Helper functions
(defun +word-wrap--major-mode-is-text-p ()
  "Return non-nil if current major mode is text-focused."
  (apply #'derived-mode-p +word-wrap-text-modes))

(defun +word-wrap--calc-extra-indent ()
  "Calculate extra indentation for wrapped lines."
  (if (+word-wrap--major-mode-is-text-p)
      0
    (let ((base-indent (or (and (boundp 'evil-shift-width) evil-shift-width)
                          (bound-and-true-p tab-width)
                          2)))
      (pcase +word-wrap-extra-indent
        ('double (* 2 base-indent))
        ('single base-indent)
        (_ 0)))))

;; Main minor mode
(define-minor-mode +word-wrap-mode
  "Smart word wrapping with language-aware indentation."
  :init-value nil
  :lighter " ↩"
  (if +word-wrap-mode
      (progn
        ;; Enable visual-line-mode
        (visual-line-mode 1)
        
        ;; Enable adaptive-wrap for smart indentation
        (require 'adaptive-wrap)
        (setq-local adaptive-wrap-extra-indent (+word-wrap--calc-extra-indent))
        (adaptive-wrap-prefix-mode 1)
        
        ;; Handle fill-column wrapping
        (when (eq +word-wrap-fill-style 'soft)
          (require 'visual-fill-column)
          (setq-local visual-fill-column-width fill-column
                      visual-fill-column-center-text nil)
          (visual-fill-column-mode 1))
        
        (when (eq +word-wrap-fill-style 'auto)
          (require 'visual-fill-column)
          (setq-local visual-fill-column-width fill-column
                      visual-fill-column-center-text nil)
          (visual-fill-column-mode 1)
          (auto-fill-mode 1)))
    
    ;; Disable
    (visual-line-mode -1)
    (adaptive-wrap-prefix-mode -1)
    (when (bound-and-true-p visual-fill-column-mode)
      (visual-fill-column-mode -1))))

;; Global mode
(defun +word-wrap--enable-for-buffer ()
  "Enable word-wrap if appropriate for current buffer."
  (unless (or (minibufferp)
              (apply #'derived-mode-p +word-wrap-disabled-modes))
    (+word-wrap-mode 1)))

(define-globalized-minor-mode +global-word-wrap-mode
  +word-wrap-mode +word-wrap--enable-for-buffer)

;; Integration with your config
;; Remove old visual-line-mode setup and add:
(add-hook 'after-init-hook #'+global-word-wrap-mode)

;; Keybindings
(ar/global-leader
  "t w" '(+word-wrap-mode :wk "Toggle Word Wrap"))
```

### Update default.nix

```nix
extraEmacsPackages = epkgs:
  with epkgs; [
    # ... existing packages
    adaptive-wrap       # Required for word-wrap
    visual-fill-column  # Required for fill-column wrapping
  ];
```

---

## 5. Lingering Projectile Configuration

### Analysis

**No projectile configuration found** in your current config. You're correctly
using `project.el` exclusively. No changes needed.

---

## 6. Flake.nix and Envrc Detection

### Analysis

Envrc automatically loads direnv environments. Your flake.nix will be detected
in subfolders **only if** those subfolders have their own `.envrc` file that
references the parent.

### Solution

**Root `.envrc` (~/Projects/sample-project/.envrc):**

```bash
use flake
watch_file flake.nix
watch_file flake.lock

# Export for all subfolders
export PROJECT_ROOT="$PWD"
export PYTHONPATH="${PWD}:${PYTHONPATH}"
```

**Subfolder `.envrc` (e.g., python-analysis/.envrc):**

```bash
# Method 1: Inherit from parent
source_up

# Method 2: Explicitly use parent flake
use flake ..
```

**Apply the configuration:**

```bash
# In root directory
cd ~/Projects/sample-project
direnv allow

# In each subfolder
cd python-analysis
direnv allow
```

---

## 7. Dynamic Tangling for Jupyter-Python

### Analysis

The `:tangle` header argument supports dynamic evaluation using elisp
expressions and can be set globally via `#+PROPERTY` or per-block.

### Implementation

```elisp
;; Add to Jupyter Configuration section
(with-eval-after-load 'org
  ;; Set default tangle for jupyter-python blocks
  (setq org-babel-default-header-args:jupyter-python
        '((:async . "yes")
          (:session . "py")
          (:kernel . "python3")
          (:tangle . "no")))  ; Default to no tangling

  ;; Function to dynamically set tangle target
  (defun my/org-babel-tangle-to-python-subdir ()
    "Tangle current jupyter-python block to python/ subdirectory."
    (interactive)
    (let* ((file-name (read-string "Python filename (without .py): "))
           (python-dir (expand-file-name "python" default-directory))
           (full-path (expand-file-name (concat file-name ".py") python-dir)))
      (unless (file-directory-p python-dir)
        (make-directory python-dir t))
      ;; Set property for current block
      (org-set-property "header-args" (format ":tangle %s" full-path))
      (message "Set tangle target to: %s" full-path))))

;; Keybinding
(ar/global-leader
  "o t" '(:ignore t :wk "tangle")
  "o t p" '(my/org-babel-tangle-to-python-subdir :wk "Set Python tangle path"))
```

### Usage Examples

```org
# Method 1: Use #+PROPERTY at file level
#+PROPERTY: header-args:jupyter-python :tangle python/analysis.py

# Method 2: Use per-block with dynamic path
#+begin_src jupyter-python :tangle (concat default-directory "python/" "analysis.py")
import pandas as pd
#+end_src

# Method 3: Use tree properties for project-specific control
* Analysis Code
:PROPERTIES:
:header-args:jupyter-python: :tangle python/analysis.py
:END:

#+begin_src jupyter-python
# This will tangle to python/analysis.py
#+end_src
```

---

## 8. Project.el Configuration for ~/Projects

### Implementation

```elisp
;; Update existing Project.el section
(use-package project
  :ensure nil
  :bind-keymap
  ("C-c p" . project-prefix-map)
  :custom
  (project-vc-extra-root-markers '(".envrc" ".project" "pyproject.toml" "Cargo.toml" ".git"))
  (project-list-file (expand-file-name "projects" no-littering-var-directory))
  
  :config
  ;; Set default projects directory
  (defcustom my/projects-directory (expand-file-name "~/Projects/")
    "Base directory for all projects."
    :type 'directory
    :group 'project)

  ;; Ensure projects directory exists
  (unless (file-directory-p my/projects-directory)
    (make-directory my/projects-directory t))

  ;; Auto-discover projects in ~/Projects
  (defun my/project-discover-projects ()
    "Discover all projects in ~/Projects directory."
    (interactive)
    (let ((subdirs (directory-files my/projects-directory t "^[^.]")))
      (dolist (dir subdirs)
        (when (and (file-directory-p dir)
                   (or (file-exists-p (expand-file-name ".git" dir))
                       (file-exists-p (expand-file-name ".project" dir))
                       (file-exists-p (expand-file-name "flake.nix" dir))))
          (project-remember-project (project-current nil dir))
          (message "Discovered project: %s" dir)))))

  ;; Run discovery on startup
  (add-hook 'after-init-hook #'my/project-discover-projects)

  ;; Helper functions (keep existing ones)
  (defun my/project-bibliography-files ()
    "Get bibliography files for current project."
    (when-let* ((project (project-current))
                (root (project-root project)))
      (let ((bib-files '()))
        (dolist (file (directory-files root t "\\.bib\\'"))
          (push file bib-files))
        (let ((ref-dir (expand-file-name "references/" root)))
          (when (file-directory-p ref-dir)
            (dolist (file (directory-files ref-dir t "\\.bib\\'"))
              (push file bib-files))))
        (let ((bib-dir (expand-file-name "bib/" root)))
          (when (file-directory-p bib-dir)
            (dolist (file (directory-files bib-dir t "\\.bib\\'"))
              (push file bib-files))))
        (nreverse bib-files))))

  ;; ... keep other helper functions ...
  )

;; Update keybindings
(ar/global-leader
  "p +" '(my/project-discover-projects :wk "Discover projects"))
```

---

## 9. Property-Based Header Arguments for Mixed LaTeX/Python Projects

### Sophisticated Implementation

This implements a production-ready system for scientific documents mixing LaTeX
writing with Python/Jupyter analysis.

```elisp
;; Add to Org Mode configuration section
(with-eval-after-load 'org
  ;; Project structure helper
  (defun my/org-setup-project-structure ()
    "Create project subfolder structure for tangling."
    (interactive)
    (let* ((org-dir (file-name-directory (buffer-file-name)))
           (python-dir (expand-file-name "python/" org-dir))
           (tex-dir (expand-file-name "tex/" org-dir))
           (output-dir (expand-file-name "output/" org-dir)))
      (dolist (dir (list python-dir tex-dir output-dir))
        (unless (file-directory-p dir)
          (make-directory dir t)))
      (message "Created: python/, tex/, output/ directories")))

  ;; Template insertion for scientific projects
  (defun my/org-insert-scientific-project-template ()
    "Insert header template for scientific org document."
    (interactive)
    (goto-char (point-min))
    (insert "#+TITLE: Scientific Analysis\n")
    (insert "#+AUTHOR: " user-full-name "\n")
    (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n")
    (insert "#+OPTIONS: toc:nil\n\n")
    (insert "# LaTeX configuration\n")
    (insert "#+LATEX_CLASS: article\n")
    (insert "#+LATEX_HEADER: \\usepackage{amsmath}\n")
    (insert "#+LATEX_HEADER: \\usepackage{graphicx}\n\n")
    (insert "# LaTeX blocks: export for PDF, never evaluate\n")
    (insert "#+PROPERTY: header-args:latex :exports code :eval never\n\n")
    (insert "# Python blocks: export results, tangle to python/\n")
    (insert "#+PROPERTY: header-args:python :session py :exports results :eval yes :tangle python/analysis.py\n\n")
    (insert "# Jupyter blocks: export results, tangle to python/\n")
    (insert "#+PROPERTY: header-args:jupyter-python :session py :async yes :exports results :eval yes :tangle python/jupyter.py :kernel python3\n\n")
    (message "Inserted scientific project template"))

  ;; Toggle Python export mode
  (defun my/org-toggle-python-export ()
    "Toggle between exporting Python results vs. hiding them entirely."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward 
           "^#\\+PROPERTY: header-args:python.*:exports none" nil t)
          (progn
            (beginning-of-line)
            (kill-line)
            (insert "#+PROPERTY: header-args:python :session py :exports results :eval yes :tangle python/analysis.py")
            (message "Python blocks will now be EXPORTED to PDF"))
        (when (re-search-forward 
               "^#\\+PROPERTY: header-args:python" nil t)
          (beginning-of-line)
          (kill-line)
          (insert "#+PROPERTY: header-args:python :session py :exports none :eval yes :tangle python/analysis.py")
          (message "Python blocks will NOT be exported to PDF")))))

  ;; Keybindings
  (ar/global-leader
    "o p" '(:ignore t :wk "properties")
    "o p s" '(my/org-insert-scientific-project-template :wk "Scientific template")
    "o p d" '(my/org-setup-project-structure :wk "Setup project dirs")
    "o p t" '(my/org-toggle-python-export :wk "Toggle Python export")))
```

### Example Org File Structure

```org
#+TITLE: Machine Learning Analysis
#+AUTHOR: Your Name
#+DATE: 2025-01-01
#+OPTIONS: toc:nil

# LaTeX settings
#+LATEX_CLASS: article

# Language-specific header arguments
#+PROPERTY: header-args:latex :exports code :eval never
#+PROPERTY: header-args:python :session py :exports results :eval yes :tangle python/analysis.py
#+PROPERTY: header-args:jupyter-python :session py :async yes :exports results :eval yes :tangle python/jupyter.py

* Introduction

#+begin_src latex
\section{Introduction}
We analyze the dataset using machine learning.
#+end_src

* Data Analysis

#+begin_src python
import pandas as pd
data = pd.read_csv('data.csv')
print(f"Loaded {len(data)} rows")
#+end_src

* Private Analysis Section
:PROPERTIES:
:header-args:python: :session py :exports none :eval yes :tangle python/private.py
:END:

#+begin_src python
# This won't appear in PDF but will still tangle
internal_metrics = calculate_metrics(data)
#+end_src
```

---

## 10. Bidi Performance Optimizations

### Analysis

Your current method **partially** achieves the performance optimizations but not
completely.

### Issue

Using `setq-local` in the global scope doesn't actually create buffer-local
variables - it needs to be in a function or hook.

### Correct Implementation

```elisp
;; Replace Performance Tuning section
(use-package so-long
  :ensure t
  :hook (emacs-startup . so-long-mode))

;; Global bidi settings (affects all buffers by default)
(setq-default bidi-display-reordering nil)

;; Org-mode specific optimizations (buffer-local)
(defun my/org-bidi-optimizations ()
  "Apply bidi optimizations for org-mode buffers."
  (setq-local bidi-paragraph-direction 'left-to-right)
  (setq-local bidi-inhibit-bpa t))

(add-hook 'org-mode-hook #'my/org-bidi-optimizations)
(add-hook 'text-mode-hook #'my/org-bidi-optimizations)
(add-hook 'prog-mode-hook #'my/org-bidi-optimizations)
```

This achieves the **same performance optimizations** as the lambda approach.

---

## 11. Jupyter Configuration with-eval-after-load

### Analysis

The `with-eval-after-load 'ob-jupyter` **is not strictly necessary** but is good
practice.

### Recommendation

**Keep it** for the following reasons:

1. Ensures functions are only called after ob-jupyter is loaded
2. Prevents errors if jupyter package fails to load
3. Follows Doom Emacs patterns for deferred configuration

### Optimized Version

```elisp
(use-package jupyter
  :ensure t
  :after org
  :commands (jupyter-run-repl jupyter-connect-repl)
  :config
  (setq org-babel-default-header-args:jupyter-python 
        '((:async . "yes")
          (:session . "py")
          (:kernel . "python3")
          (:tangle . "no")))

  ;; Keep this wrapper for safety
  (with-eval-after-load 'ob-jupyter
    (org-babel-jupyter-override-src-block "python")
    (setq jupyter-repl-echo-eval-p t))

  ;; Keybindings remain the same
  )
```

---

## 12. Project-Specific Citar Configuration for Org-Mode

### Enhanced Implementation

```elisp
(use-package citar
  :ensure t
  :custom
  (citar-bibliography '("~/references.bib"))
  (citar-library-paths '("~/Zotero/storage"))
  (citar-notes-paths (list my/org-roam-directory))

  (citar-symbols
   `((file ,(nerd-icons-mdicon "nf-md-file_document") . " ")
     (note ,(nerd-icons-mdicon "nf-md-note_text") . " ")
     (link ,(nerd-icons-mdicon "nf-md-link") . " ")))

  :config
  ;; Project bibliography detection
  (defun my/citar-get-project-bibliographies ()
    "Get bibliography files for current project."
    (when-let* ((project (project-current))
                (root (project-root project)))
      (let ((bib-files '()))
        (dolist (file (directory-files root t "\\.bib\\'"))
          (push file bib-files))
        (let ((ref-dir (expand-file-name "references/" root)))
          (when (file-directory-p ref-dir)
            (dolist (file (directory-files ref-dir t "\\.bib\\'"))
              (push file bib-files))))
        (let ((bib-dir (expand-file-name "bib/" root)))
          (when (file-directory-p bib-dir)
            (dolist (file (directory-files bib-dir t "\\.bib\\'"))
              (push file bib-files))))
        (nreverse bib-files))))

  (defun my/citar-bibliography ()
    "Return project-specific bibliography or global fallback."
    (or (my/citar-get-project-bibliographies)
        citar-bibliography))

  ;; Override citar's bibliography function
  (advice-add 'citar--bibliography-files :override #'my/citar-bibliography)

  ;; Set buffer-local bibliography
  (defun my/citar-set-local-bibliography ()
    "Set buffer-local bibliography when opening org/LaTeX files."
    (when (and (buffer-file-name)
               (or (derived-mode-p 'org-mode)
                   (derived-mode-p 'LaTeX-mode)))
      (setq-local citar-bibliography (my/citar-bibliography))
      (when (derived-mode-p 'org-mode)
        (setq-local org-cite-global-bibliography (my/citar-bibliography)))))

  (add-hook 'org-mode-hook #'my/citar-set-local-bibliography)
  (add-hook 'LaTeX-mode-hook #'my/citar-set-local-bibliography)

  :hook
  ((LaTeX-mode org-mode) . (lambda ()
                             (local-set-key (kbd "C-c b") #'citar-insert-citation))))

;; Ensure org-cite uses citar
(with-eval-after-load 'org
  (setq org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar)
  
  (defun my/update-org-bibliography ()
    "Update org-cite bibliography when entering org buffer."
    (when (derived-mode-p 'org-mode)
      (setq-local org-cite-global-bibliography (my/citar-bibliography))))
  
  (add-hook 'org-mode-hook #'my/update-org-bibliography))

;; Citar-Embark and Citar-Org-Roam remain the same
(use-package citar-embark
  :ensure t
  :after (citar embark)
  :config (citar-embark-mode))

(use-package citar-org-roam
  :ensure t
  :after (citar org-roam)
  :config (citar-org-roam-mode 1))
```

---

## Summary of Changes

### Critical Changes

1. **Keep org-src-fontify-natively enabled** - tree-sitter works through it
2. **Implement basic Doom evil behaviors** - Y, o/O, visual search
3. **Add Doom-style word wrapping** - with adaptive indentation
4. **Fix bidi optimizations** - use hooks properly
5. **Enhance citar org-mode integration** - ensure project-specific works

### Recommended Additions

1. **evil-surround and evil-matchit** - complete vim experience
2. **Project discovery** - auto-find projects in ~/Projects
3. **Dynamic tangling** - interactive Python file targeting
4. **Property-based headers** - sophisticated LaTeX/Python mixing

### Optional Improvements

1. Keep `with-eval-after-load 'ob-jupyter` for safety
2. Use `.envrc` inheritance for flake.nix in subfolders
3. Add project-specific keybindings for file finding

All implementations integrate cleanly with your existing lsp-bridge, evil, and
org-mode setup.
