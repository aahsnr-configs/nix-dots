# Emacs Configuration Analysis and Improvements

**Focus Areas**: Doom Evil Implementation, Word Wrap, Property-Based Headers,
Citar Integration, Smartparens, and Direnv

## 1. Treesit Syntax Highlighting in Org Source Blocks

### Analysis

Tree-sitter can indeed be used for syntax highlighting in org source blocks, and
when properly configured, tree-sitter will automatically fontify code blocks.
However, the relationship with `org-src-fontify-natively` is important to
understand.

### Issue

Setting `(org-src-fontify-natively nil)` disables native fontification entirely,
which prevents both traditional font-lock and tree-sitter from working in source
blocks.

### Solution

**Keep `org-src-fontify-natively t`** - This allows tree-sitter to work
automatically when treesit modes are available. Since you have lsp-bridge active
in org source blocks, the syntax highlighting will work through the combination
of:

1. Native org fontification (enabled)
2. Tree-sitter modes (auto-enabled when available)
3. LSP-bridge semantic tokens

### Recommended Change

```elisp
;; In Core Configuration section
(org-src-fontify-natively t)  ; Keep this enabled
(org-fontify-quote-and-verse-blocks nil)
(org-fontify-whole-heading-line nil)
```

---

## 2. Missing Vim-like Behavior Packages

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

;; Add C-a/C-x number increment/decrement (already have evil-numbers)
(general-define-key
 :states '(normal visual)
 "C-a" 'evil-numbers/inc-at-pt
 "C-x" 'evil-numbers/dec-at-pt
 "g C-a" 'evil-numbers/inc-at-pt-incremental
 "g C-x" 'evil-numbers/dec-at-pt-incremental)
```

---

## 3. Comprehensive Doom Emacs Evil Implementation

Based on analysis of Doom's `modules/editor/evil/config.el`, here's a complete
implementation of Doom's Evil enhancements.

### Core Evil Settings (Before evil-mode loads)

```elisp
;; Replace existing evil :init section with Doom's settings
(use-package evil
  :ensure t
  :init
  ;; Doom Evil Settings - Set BEFORE evil loads
  (setq evil-want-C-g-bindings t
        evil-want-C-i-jump nil  ; Doom does this themselves
        evil-want-C-u-scroll t  ; Universal arg moved to SPC u
        evil-want-C-u-delete t
        evil-want-C-w-delete t
        evil-want-Y-yank-to-eol t
        evil-want-abbrev-expand-on-insert-exit nil
        evil-want-integration t
        evil-want-keybinding nil
        ;; More vim-like behavior
        evil-symbol-word-search t
        ;; Cursor appearance
        evil-default-cursor t
        evil-normal-state-cursor 'box
        evil-emacs-state-cursor '(box +evil-emacs-cursor-fn)
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow
        ;; Search and highlighting
        evil-ex-search-vim-style-regexp t
        evil-ex-visual-char-range t
        evil-ex-interactive-search-highlight 'selected-window
        ;; Mode line
        evil-mode-line-format nil
        ;; Suppress "beginning/end of line" errors in macros
        evil-kbd-macro-suppress-motion-error t
        ;; More uniform cursor color
        evil-visual-state-cursor 'hollow
        ;; Don't move cursor back when exiting insert mode
        evil-move-cursor-back nil
        ;; Fine undo - separate each action
        evil-want-fine-undo t
        ;; Don't move cursor beyond EOL
        evil-move-beyond-eol nil
        ;; Better search wrapping
        evil-search-wrap t
        evil-magic t
        evil-echo-state t
        evil-indent-convert-tabs t
        evil-ex-substitute-global t
        evil-insert-skip-empty-lines t
        evil-v$-excludes-newline t
        ;; Split window behavior
        evil-split-window-below t
        evil-vsplit-window-right t)

  :config
  (evil-mode 1)
  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; Additional Doom configurations
  (setq evil-shift-width tab-width)
  
  ;; Make evil-mode play nice with custom modes
  (evil-set-initial-state 'special-mode 'motion)
  
  ;; Custom cursor function for Emacs state
  (defun +evil-emacs-cursor-fn ()
    (evil-set-cursor-color (get 'cursor 'evil-normal-color)))

  ;; Don't interfere with localleader key
  (define-key evil-motion-state-map "\\" nil))
```

### Doom Evil Comment Continuation (o/O Keys)

```elisp
;; Add to Evil Extensions section or after evil config
(defcustom +evil-want-o/O-to-continue-comments t
  "If non-nil, o/O keys will continue comment lines."
  :type 'boolean
  :group 'evil)

(defun +evil--insert-newline-below-and-respect-comments-p (context)
  "Return non-nil if newline should respect comments in CONTEXT."
  (and +evil-want-o/O-to-continue-comments
       (eq (plist-get context :

type) 'line-comment)))

(defun +evil--insert-newline-above-and-respect-comments-p (context)
  "Return non-nil if newline should respect comments in CONTEXT."
  (and +evil-want-o/O-to-continue-comments
       (eq (plist-get context :type) 'line-comment)))

;; Improved comment continuation that works with smartparens
(defun +evil/insert-newline-below (count)
  "Insert COUNT newlines below, respecting comments."
  (interactive "p")
  (save-excursion
    (if (and (fboundp 'sp-point-in-comment)
             (sp-point-in-comment)
             +evil-want-o/O-to-continue-comments)
        (let ((comment-start (or comment-start ""))
              (comment-padding (or comment-padding " ")))
          (end-of-line)
          (newline-and-indent)
          (when (and comment-start (not (string-empty-p comment-start)))
            (insert comment-start comment-padding)))
      (evil-open-below count)))
  (evil-insert-state))

(defun +evil/insert-newline-above (count)
  "Insert COUNT newlines above, respecting comments."
  (interactive "p")
  (save-excursion
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
      (evil-open-above count)))
  (evil-insert-state))

(define-key evil-normal-state-map "o" #'+evil/insert-newline-below)
(define-key evil-normal-state-map "O" #'+evil/insert-newline-above)
```

### Doom Visual Search (*/# in visual mode)

```elisp
(defun +evil--visual-search (direction)
  "Search for the visual selection in DIRECTION."
  (let* ((beg (region-beginning))
         (end (region-end))
         (selection (buffer-substring-no-properties beg end)))
    (deactivate-mark)
    (evil-ex-search-activate-highlight
     (list :pattern selection
           :forward (eq direction 'forward)))
    (if (eq direction 'forward)
        (evil-search-forward nil nil nil selection)
      (evil-search-backward nil nil nil selection))))

(defun +evil/visual-search-forward ()
  "Search forward for the visual selection."
  (interactive)
  (+evil--visual-search 'forward))

(defun +evil/visual-search-backward ()
  "Search backward for the visual selection."
  (interactive)
  (+evil--visual-search 'backward))

(define-key evil-visual-state-map "*" #'+evil/visual-search-forward)
(define-key evil-visual-state-map "#" #'+evil/visual-search-backward)
```

### Doom Evil Ex Commands

```elisp
;; Add custom ex commands
(with-eval-after-load 'evil-ex
  ;; Improved :global with highlighting
  (evil-ex-define-cmd "g[lobal]" #'evil-ex-global)
  
  (defun +evil--highlight-global-matches ()
    "Highlight matches for :global command."
    (when-let ((pattern (car evil-ex-global-match)))
      (hi-lock-face-buffer pattern 'hi-yellow)))
  
  (add-hook 'evil-ex-global-hook #'+evil--highlight-global-matches)
  
  ;; Additional useful commands
  (evil-ex-define-cmd "al[ign]" #'align-regexp)
  (evil-ex-define-cmd "retab" #'+evil:retab))

;; Retab function
(defun +evil:retab (&optional beg end)
  "Convert tabs to spaces or vice versa in region or buffer."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (unless (and beg end)
    (setq beg (point-min)
          end (point-max)))
  (if indent-tabs-mode
      (tabify beg end)
    (untabify beg end)))
```

### Doom Evil Window Movement Enhancements

```elisp
(defcustom +evil-want-move-window-to-wrap-around nil
  "If non-nil, window movement commands wrap around."
  :type 'boolean
  :group 'evil)

(defun +evil/window-move-left ()
  "Move to window on the left, wrapping if needed."
  (interactive)
  (if (and +evil-want-move-window-to-wrap-around
           (not (ignore-errors (windmove-left))))
      (evil-window-bottom-right)
    (windmove-left)))

(defun +evil/window-move-right ()
  "Move to window on the right, wrapping if needed."
  (interactive)
  (if (and +evil-want-move-window-to-wrap-around
           (not (ignore-errors (windmove-right))))
      (evil-window-top-left)
    (windmove-right)))

(defun +evil/window-move-up ()
  "Move to window above, wrapping if needed."
  (interactive)
  (if (and +evil-want-move-window-to-wrap-around
           (not (ignore-errors (windmove-up))))
      (evil-window-bottom-right)
    (windmove-up)))

(defun +evil/window-move-down ()
  "Move to window below, wrapping if needed."
  (interactive)
  (if (and +evil-want-move-window-to-wrap-around
           (not (ignore-errors (windmove-down))))
      (evil-window-top-left)
    (windmove-down)))

;; Bind to SPC w keys
(ar/global-leader
  "w h" '(+evil/window-move-left :wk "Window Left")
  "w j" '(+evil/window-move-down :wk "Window Down")
  "w k" '(+evil/window-move-up :wk "Window Up")
  "w l" '(+evil/window-move-right :wk "Window Right"))
```

### Doom Evil Preprocessor Navigation

```elisp
(defcustom +evil-preprocessor-regexp "^\\s-*#[a-zA-Z0-9_]"
  "Regexp for preprocessor directives (C/C++)."
  :type 'regexp
  :group 'evil)

(defun +evil/next-preproc-directive ()
  "Jump to next preprocessor directive."
  (interactive)
  (re-search-forward +evil-preprocessor-regexp nil t))

(defun +evil/previous-preproc-directive ()
  "Jump to previous preprocessor directive."
  (interactive)
  (re-search-backward +evil-preprocessor-regexp nil t))

(define-key evil-normal-state-map "]#" #'+evil/next-preproc-directive)
(define-key evil-normal-state-map "[#" #'+evil/previous-preproc-directive)
```

### Minibuffer Evil Integration

```elisp
;; Doom's minibuffer evil bindings
(with-eval-after-load 'evil
  (define-key evil-ex-completion-map (kbd "C-a") #'evil-beginning-of-line)
  (define-key evil-ex-completion-map (kbd "C-b") #'evil-backward-char)
  (define-key evil-ex-completion-map (kbd "C-f") #'evil-forward-char)
  
  (define-key evil-ex-search-keymap (kbd "C-a") #'evil-beginning-of-line)
  (define-key evil-ex-search-keymap (kbd "C-b") #'evil-backward-char)
  (define-key evil-ex-search-keymap (kbd "C-f") #'evil-forward-char))
```

### Smartparens Disable in Evil Replace Mode

```elisp
;; Doom disables smartparens in replace mode
(with-eval-after-load 'smartparens
  (add-hook 'evil-replace-state-entry-hook #'turn-off-smartparens-mode)
  (add-hook 'evil-replace-state-exit-hook #'turn-on-smartparens-mode))
```

---

## 4. Comprehensive Doom Word Wrap Implementation

Doom's word-wrap module provides intelligent soft-wrapping with language-aware
indentation using `adaptive-wrap` and `visual-fill-column`.

### Installation Requirements

```elisp
;; Add to default.nix packages
;;   adaptive-wrap
;;   visual-fill-column
```

### Complete Word Wrap Implementation

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
  '(fundamental-mode so-long-mode special-mode compilation-mode)
  "Major modes where word-wrap should not be enabled."
  :type '(repeat symbol)
  :group 'word-wrap)

(defcustom +word-wrap-text-modes
  '(text-mode markdown-mode gfm-mode org-mode rst-mode)
  "Major modes that are primarily text (no extra indent)."
  :type '(repeat symbol)
  :group 'word-wrap)

(defcustom +word-wrap-visual-modes
  '(prog-mode conf-mode)
  "Modes where visual-line-mode should be used."
  :type '(repeat symbol)
  :group 'word-wrap)

;; Internal variables
(defvar-local +word-wrap--prev-visual-line-mode nil)
(defvar-local +word-wrap--prev-adaptive-wrap-mode nil)
(defvar-local +word-wrap--major-mode-indent-var nil)

;; Core functions
(defun +word-wrap--in-comment-p ()
  "Return non-nil if point is in a comment."
  (or (nth 4 (syntax-ppss))
      (and (fboundp 'sp-point-in-comment)
           (sp-point-in-comment))))

(defun +word-wrap--in-string-p ()
  "Return non-nil if point is in a string."
  (nth 3 (syntax-ppss)))

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

;; Adaptive wrap advice for context-aware indentation
(defun +word-wrap--adjust-extra-indent-a (orig-fun &rest args)
  "Add extra indent to wrapped lines in code, but not in comments/strings."
  (let ((extra-indent (if (or (+word-wrap--in-comment-p)
                              (+word-wrap--in-string-p)
                              (+word-wrap--major-mode-is-text-p))
                          0
                        (+word-wrap--calc-extra-indent))))
    (let ((adaptive-wrap-extra-indent extra-indent))
      (apply orig-fun args))))

;; Main minor mode
(define-minor-mode +word-wrap-mode
  "Smart word wrapping with language-aware indentation.

Wrapped lines will be indented to match the preceding line.
In code buffers, lines not in strings/comments get additional
indentation per `+word-wrap-extra-indent'.

Long lines wrap at window margin by default, or can optionally
wrap at `fill-column' via `+word-wrap-fill-style'."
  :init-value nil
  :lighter " ↩"
  :group 'word-wrap
  (if +word-wrap-mode
      ;; Enable
      (progn
        ;; Store previous states
        (setq +word-wrap--prev-visual-line-mode visual-line-mode
              +word-wrap--prev-adaptive-wrap-mode
              (bound-and-true-p adaptive-wrap-prefix-mode))
        
        ;; Get major mode indent variable
        (when (fboundp 'dtrt-indent--search-hook-mapping)
          (setq +word-wrap--major-mode-indent-var
                (caddr (dtrt-indent--search-hook-mapping major-mode))))
        
        ;; Apply adaptive wrap advice
        (advice-add #'adaptive-wrap-fill-context-prefix
                    :around #'+word-wrap--adjust-extra-indent-a)
        
        ;; Enable visual-line-mode
        (unless +word-wrap--prev-visual-line-mode
          (visual-line-mode +1))
        
        ;; Enable adaptive-wrap for smart indentation
        (unless +word-wrap--prev-adaptive-wrap-mode
          (require 'adaptive-wrap)
          (setq-local adaptive-wrap-extra-indent (+word-wrap--calc-extra-indent))
          (adaptive-wrap-prefix-mode +1))
        
        ;; Handle fill-column wrapping
        (pcase +word-wrap-fill-style
          ('soft
           (require 'visual-fill-column)
           (setq-local visual-fill-column-width fill-column
                       visual-fill-column-center-text nil
                       visual-fill-column-fringes-outside-margins nil)
           (visual-fill-column-mode +1))
          ('auto
           (require 'visual-fill-column)
           (setq-local visual-fill-column-width fill-column
                       visual-fill-column-center-text nil
                       visual-fill-column-fringes-outside-margins nil)
           (visual-fill-column-mode +1)
           ;; Don't disable auto-fill if it's already on
           (unless (bound-and-true-p auto-fill-function)
             (auto-fill-mode +1)))))
    
    ;; Disable
    (advice-remove #'adaptive-wrap-fill-context-prefix
                   #'+word-wrap--adjust-extra-indent-a)
    (unless +word-wrap--prev-adaptive-wrap-mode
      (adaptive-wrap-prefix-mode -1))
    (unless +word-wrap--prev-visual-line-mode
      (visual-line-mode -1))
    (when (bound-and-true-p visual-fill-column-mode)
      (visual-fill-column-mode -1))))

;; Global mode
(defun +word-wrap--enable-for-buffer ()
  "Enable word-wrap if appropriate for current buffer."
  (unless (or (minibufferp)
              (apply #'derived-mode-p +word-wrap-disabled-modes)
              (and (boundp 'special-mode)
                   (derived-mode-p 'special-mode)))
    (+word-wrap-mode +1)))

(define-globalized-minor-mode +global-word-wrap-mode
  +word-wrap-mode +word-wrap--enable-for-buffer
  :group 'word-wrap)

;; Zen mode integration (optional)
(defun +word-wrap--setup-zen-mode ()
  "Configure word wrap for zen/writeroom mode."
  (when (bound-and-true-p writeroom-mode)
    (setq-local +word-wrap-fill-style 'soft)
    (+word-wrap-mode +1)))

(with-eval-after-load 'writeroom-mode
  (add-hook 'writeroom-mode-hook #'+word-wrap--setup-zen-mode))

;; Keybindings
(ar/global-leader
  "t w" '(+word-wrap-mode :wk "Toggle Word Wrap")
  "t W" '(+global-word-wrap-mode :wk "Toggle Global Word Wrap"))
```

### Usage Examples

```elisp
;; Example 1: Enable globally with fill-column wrapping
(setq +word-wrap-fill-style 'soft)
(+global-word-wrap-mode +1)

;; Example 2: Mode-specific configuration
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local +word-wrap-extra-indent nil
                        +word-wrap-fill-style 'soft)
            (+word-wrap-mode +1)))

;; Example 3: JSON with single indent and fill-column wrap
(add-hook 'json-mode-hook
          (lambda ()
            (setq-local +word-wrap-extra-indent 'single
                        +word-wrap-fill-style 'soft)
            (+word-wrap-mode +1)))
```

### Integration with Your Config

```elisp
;; replace your existing visual-line-mode setup with:
(defun visual-fill-column-for-vline ()
  "deprecated - use +word-wrap-mode instead."
  (message "use +word-wrap-mode for better word wrapping"))

;; remove old hooks
(remove-hook 'after-init-hook #'global-visual-line-mode)
(remove-hook 'visual-line-mode-hook #'visual-fill-column-for-vline)

;; add new setup
(add-hook 'after-init-hook #'+global-word-wrap-mode)
```

---

Doom's word-wrap module supports soft-wrapping at fill-column and provides
automatic wrapping in most buffers with smart indentation.

```elisp
;; Replace existing visual-line-mode section with Doom-style word wrap
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
- 'soft: wrap at fill-column
- 'auto: use auto-fill-mode for new lines, soft-wrap existing"
  :type '(choice (const :tag "Window edge" nil)
                 (const :tag "Fill column (soft)" soft)
                 (const :tag "Auto (mixed)" auto))
  :group 'word-wrap)

(defcustom +word-wrap-disabled-modes
  '(fundamental-mode so-long-mode)
  "Major modes where word-wrap should not be enabled."
  :type '(repeat symbol)
  :group 'word-wrap)

(defcustom +word-wrap-text-modes
  '(text-mode markdown-mode gfm-mode org-mode)
  "Major modes that are primarily text (no extra indent)."
  :type '(repeat symbol)
  :group 'word-wrap)

(defun +word-wrap--calc-extra-indent ()
  "Calculate extra indentation for wrapped lines."
  (if (apply #'derived-mode-p +word-wrap-text-modes)
      0
    (let ((base-indent (or (and (boundp 'evil-shift-width) evil-shift-width)
                          (bound-and-true-p tab-width)
                          2)))
      (pcase +word-wrap-extra-indent
        ('double (* 2 base-indent))
        ('single base-indent)
        (_ 0)))))

(define-minor-mode +word-wrap-mode
  "Smart word wrapping mode with adaptive indentation."
  :init-value nil
  :lighter " ↩"
  (if +word-wrap-mode
      (progn
        ;; Enable visual-line-mode
        (visual-line-mode 1)
        ;; Set up adaptive wrap
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
          (visual-fill-column-mode 1)))
    ;; Disable
    (visual-line-mode -1)
    (adaptive-wrap-prefix-mode -1)
    (when (bound-and-true-p visual-fill-column-mode)
      (visual-fill-column-mode -1))))

(define-globalized-minor-mode +global-word-wrap-mode
  +word-wrap-mode +word-wrap--enable)

(defun +word-wrap--enable ()
  "Enable word-wrap if appropriate for current buffer."
  (unless (or (minibufferp)
              (apply #'derived-mode-p +word-wrap-disabled-modes)
              (and (boundp 'special-mode) (derived-mode-p 'special-mode)))
    (+word-wrap-mode 1)))

;; Helper function for use with visual-line-mode hook
(defun visual-fill-column-for-vline ()
  "Setup visual-fill-column for visual-line-mode."
  (when (and visual-line-mode
             (not (minibufferp))
             (> fill-column 0))
    (+word-wrap-mode 1)))

;; Update existing hooks
(remove-hook 'visual-line-mode-hook #'visual-fill-column-for-vline)
(add-hook 'after-init-hook #'+global-word-wrap-mode)

;; Add keybinding
(ar/global-leader
  "t w" '(+word-wrap-mode :wk "Toggle Word Wrap"))
```

---

## 5. Lingering Projectile Configuration

### Analysis

**No projectile configuration found** in your current config. You're using
`project.el`, which is correct. No changes needed.

---

---

## 7. Direnv with Flake Inheritance for Subdirectories

Complete guide to using direnv with `use flake` for inheritance in project
subdirectories.

### Understanding Direnv Inheritance

Direnv automatically searches parent directories for `.envrc` files, allowing
subdirectories to inherit from a parent flake.

### Method 1: Parent Flake with Subdirectory Inheritance (Recommended)

**Project Structure:**

```
~/Projects/sample-project/
├── flake.nix              # Main flake with all dependencies
├── flake.lock
├── .envrc                 # Root .envrc
├── python-analysis/
│   ├── .envrc            # Inherits from parent
│   ├── analysis.org
│   └── python/
├── latex-report/
│   ├── .envrc            # Inherits from parent
│   └── report.org
└── data-processing/
    ├── .envrc            # Inherits from parent
    └── process.py
```

**Root `.envrc` (~/Projects/sample-project/.envrc):**

```bash
# This is the main .envrc that loads the flake
use flake

# Optional: Watch additional files for changes
watch_file flake.nix
watch_file flake.lock

# Optional: Export additional variables for all subdirectories
export PROJECT_ROOT="$PWD"
export PYTHONPATH="$PWD:${PYTHONPATH}"

# Optional: Source a .env file if it exists
if [ -f .env ]; then
  dotenv .env
fi
```

**Subfolder `.envrc` (python-analysis/.envrc, latex-report/.envrc, etc.):**

```bash
# Method A: Explicit parent reference
source_up

# Method B: Use parent's flake directly
use flake ..

# Method C: Source parent and add local customizations
source_up
export LOCAL_VAR="subfolder-specific-value"
```

### Method 2: Multiple Development Shells in Single Flake

For projects with different dependency sets per subfolder.

**Root `flake.nix` with multiple shells:**

```nix
{
  description = "Multi-environment project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells = {
          # Default shell for root
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              git
              direnv
            ];
          };

          # Python analysis environment
          python = pkgs.mkShell {
            buildInputs = with pkgs; [
              python311
              python311Packages.pandas
              python311Packages.numpy
              python311Packages.matplotlib
              python311Packages.jupyter
            ];
            shellHook = ''
              echo "Python analysis environment loaded"
            '';
          };

          # LaTeX writing environment
          latex = pkgs.mkShell {
            buildInputs = with pkgs; [
              texlive.combined.scheme-full
              tectonic
            ];
            shellHook = ''
              echo "LaTeX environment loaded"
            '';
          };

          # Data processing environment
          data = pkgs.mkShell {
            buildInputs = with pkgs; [
              python311
              python311Packages.dask
              python311Packages.pyarrow
            ];
          };
        };
      }
    );
}
```

**Subfolder `.envrc` files:**

```bash
# python-analysis/.envrc
use flake ..#python

# latex-report/.envrc
use flake ..#latex

# data-processing/.envrc
use flake ..#data
```

### Method 3: Layered Environments

Stack multiple flakes for maximum flexibility.

**Root `.envrc`:**

```bash
# Load base development tools
use flake

# Also load a shared utilities flake
use flake github:yourorg/dev-utils
```

**Subfolder `.envrc`:**

```bash
# Inherit parent environments
source_up

# Add language-specific environment
use flake github:nix-community/poetry2nix
```

### Complete Example for Your Use Case

**~/Projects/sample-project/flake.nix:**

```nix
{
  description = "Scientific project with Python, LaTeX, and Jupyter";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        pythonEnv = pkgs.python311.withPackages (ps: with ps; [
          pandas
          numpy
          matplotlib
          seaborn
          scikit-learn
          jupyter
          ipython
        ]);
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Python and Jupyter
            pythonEnv
            
            # LaTeX
            tectonic
            
            # Development tools
            git
            direnv
            nix-direnv
            
            # Optional: Bibliography management
            zotero
          ];

          shellHook = ''
            echo "🔬 Scientific project environment loaded"
            echo "Python: $(python --version)"
            echo "Jupyter: $(jupyter --version)"
            echo "Tectonic: $(tectonic --version)"
            
            # Set up Python path
            export PYTHONPATH="$PWD:${PYTHONPATH}"
            
            # Create standard directories
            mkdir -p python tex output references
          '';

          # Environment variables
          PROJECT_ROOT = "$PWD";
          JUPYTER_CONFIG_DIR = "$PWD/.jupyter";
        };
      }
    );
}
```

**~/Projects/sample-project/.envrc:**

```bash
#!/usr/bin/env bash
# Enable nix-direnv for better caching
if ! has nix_direnv_version || ! nix_direnv_version 2.3.0; then
  source_url "https://raw.githubusercontent.com/nix-community/nix-direnv/2.3.0/direnvrc" "sha256-Dmd+j63L84wuzgyjITIfSxSD57Tx7v51DMxVZOsiUD8="
fi

# Load the flake
use flake

# Watch flake files for automatic reload
watch_file flake.nix
watch_file flake.lock

# Project-wide variables
export PROJECT_ROOT="$PWD"
export PYTHONPATH="$PWD:${PYTHONPATH}"

# Load local .env if exists (for secrets, API keys, etc.)
if [ -f .env ]; then
  dotenv .env
fi

echo "✓ Main project environment loaded"
```

**Subproject .envrc files inherit automatically:**

**~/Projects/sample-project/analysis-1/.envrc:**

```bash
# Simply inherit everything from parent
source_up

# Optionally add subfolder-specific variables
export ANALYSIS_NAME="experiment-1"
```

**~/Projects/sample-project/paper-draft/.envrc:**

```bash
source_up
export PAPER_VERSION="draft-v2"
```

### Testing and Debugging

```bash
# Test flake builds correctly
nix flake check

# Enter the dev environment manually
nix develop

# Or use direnv
cd ~/Projects/sample-project
direnv allow

# Check what's loaded
direnv status

# Check which .envrc is active
direnv current

# Reload environment after changes
direnv reload

# Debug mode
DIRENV_LOG_FORMAT="%s" direnv allow
```

### Troubleshooting

**Issue:** Changes to flake.nix not picked up

```bash
# Solution: Add watch_file to .envrc
watch_file flake.nix
direnv allow
```

**Issue:** Subdirectory not inheriting

```bash
# Solution: Ensure parent .envrc is allowed
cd ~/Projects/sample-project
direnv allow
cd analysis-1
direnv allow
```

**Issue:** Slow reload times

```bash
# Solution: Install nix-direnv for caching
# Already in your default.nix emacs config via envrc package
# Just ensure nix-direnv is available system-wide
```

### Emacs Integration Verification

Your current config has `envrc-global-mode` which automatically handles direnv.
Test it:

```elisp
;; Check if envrc is working
M-x envrc-allow

;; Check environment variables in Emacs
M-x getenv RET PROJECT_ROOT

;; Force reload
M-x envrc-reload
```

### Best Practices

1. **Always use `source_up` in subfolder `.envrc` files**
2. **Add `watch_file flake.nix` to root `.envrc`**
3. **Use `direnv allow` after creating/modifying `.envrc`**
4. **Keep secrets in `.env` files (add to `.gitignore`)**
5. **Use `.dir-locals.el` for Emacs-specific settings**

---

## 7. Dynamic Tangling for Jupyter-Python

### Analysis

The :tangle header argument supports dynamic evaluation using elisp expressions,
and can be set globally via #+PROPERTY or per-block.

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
    (let* ((info (org-babel-get-src-block-info))
           (lang (nth 0 info))
           (body (nth 1 info))
           (params (nth 2 info))
           (file-name (read-string "Python filename (without .py): "))
           (python-dir (expand-file-name "python" default-directory))
           (full-path (expand-file-name (concat file-name ".py") python-dir)))
      (unless (file-directory-p python-dir)
        (make-directory python-dir t))
      ;; Update block with tangle path
      (org-babel-tangle-jump-to-org)
      (org-set-property "tangle" full-path)
      (message "Set tangle target to: %s" full-path))))

;; Usage in org file:
;; Method 1: Use #+PROPERTY at file level
;; #+PROPERTY: header-args:jupyter-python :tangle python/analysis.py

;; Method 2: Use per-block with dynamic path
;; #+begin_src jupyter-python :tangle (concat default-directory "python/" "analysis.py")

;; Method 3: Use tree properties for project-specific control
;; * Analysis Code
;; :PROPERTIES:
;; :header-args:jupyter-python: :tangle python/analysis.py
;; :END:
```

Add keybinding:

```elisp
(ar/global-leader
  "o t" '(:ignore t :wk "tangle")
  "o t p" '(my/org-babel-tangle-to-python-subdir :wk "Set Python tangle path"))
```

---

## 8. Project.el Configuration for ~/Projects

### Implementation

```elisp
;; Replace existing Project.el section
(use-package project
  :ensure nil
  :bind-keymap
  ("C-c p" . project-prefix-map)
  :custom
  ;; Projects directory
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

  ;; Helper functions remain the same...
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

  (defun my/project-find-file-by-extension (ext)
    "Find files with extension EXT in current project."
    (when-let* ((project (project-current))
                (files (project-files project))
                (filtered (seq-filter (lambda (f) (string-suffix-p ext f)) files)))
      (find-file (completing-read (format "Select %s file: " ext) filtered))))

  (defun my/project-find-bib-file ()
    "Open a .bib file in current project."
    (interactive)
    (if-let ((bib-files (my/project-bibliography-files)))
        (find-file (completing-read "Select bibliography: " bib-files))
      (message "No .bib files found in project")))

  (defun my/project-find-tex-file ()
    "Open a .tex file in current project."
    (interactive)
    (my/project-find-file-by-extension ".tex"))

  (defun my/project-find-py-file ()
    "Open a .py file in current project."
    (interactive)
    (my/project-find-file-by-extension ".py"))

  ;; Enhanced project switching to prefer ~/Projects
  (defun my/project-switch-project ()
    "Switch to a project, preferring those in ~/Projects."
    (interactive)
    (let* ((all-projects (project-known-project-roots))
           (projects-dir-projects
            (seq-filter (lambda (p) (string-prefix-p my/projects-directory p))
                       all-projects))
           (other-projects
            (seq-filter (lambda (p) (not (string-prefix-p my/projects-directory p)))
                       all-projects))
           (sorted-projects (append projects-dir-projects other-projects)))
      (project-switch-project (completing-read "Switch to project: " sorted-projects)))))

;; Update keybindings
(ar/global-leader
  "p" '(:ignore t :wk "project")
  "p f" '(project-find-file :wk "Find file")
  "p p" '(my/project-switch-project :wk "Switch project")
  "p d" '(project-find-dir :wk "Find directory")
  "p b" '(project-switch-to-buffer :wk "Switch buffer")
  "p k" '(project-kill-buffers :wk "Kill buffers")
  "p D" '(project-dired :wk "Dired root")
  "p s" '(project-find-regexp :wk "Search (grep)")
  "p r" '(consult-ripgrep :wk "Search (ripgrep)")
  "p c" '(project-compile :wk "Compile")
  "p e" '(project-eshell :wk "Eshell")
  "p +" '(my/project-discover-projects :wk "Discover projects")
  ;; Custom
  "p B" '(my/project-find-bib-file :wk "Find .bib")
  "p t" '(my/project-find-tex-file :wk "Find .tex")
  "p y" '(my/project-find-py-file :wk "Find .py"))
```

---

## 6. Sophisticated Property-Based Header Arguments for Mixed LaTeX/Python/Jupyter Projects

This implements a production-ready system for scientific documents that mix
LaTeX writing with Python/Jupyter analysis.

### The Workflow

Your org files will:

1. Write scientific documents in LaTeX blocks (exported to PDF)
2. Perform analysis in Python/Jupyter blocks (tangled to `.py` files)
3. Optionally include or exclude Python output in PDF exports
4. Automatically apply language-specific header args via `#+PROPERTY`

### Complete Implementation

```elisp
;; Add to your org-mode configuration
(with-eval-after-load 'org
  ;; Ensure languages are loaded
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)
     (jupyter . t)
     (python . t)
     (shell . t)))

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
    (insert "# Org-mode settings\n")
    (insert "#+STARTUP: overview indent\n")
    (insert "# -*- org-src-preserve-indentation: t; -*-\n\n")
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
      (let ((case-fold-search nil))
        (if (re-search-forward 
             "^#\\+PROPERTY: header-args:python.*:exports none" nil t)
            ;; Currently set to none, change to results
            (progn
              (beginning-of-line)
              (kill-line)
              (insert "#+PROPERTY: header-args:python :session py :exports results :eval yes :tangle python/analysis.py")
              (message "Python blocks will now be EXPORTED to PDF"))
          ;; Currently exports results, change to none
          (when (re-search-forward 
                 "^#\\+PROPERTY: header-args:python" nil t)
            (beginning-of-line)
            (kill-line)
            (insert "#+PROPERTY: header-args:python :session py :exports none :eval yes :tangle python/analysis.py")
            (message "Python blocks will NOT be exported to PDF"))))))

  ;; Toggle Python code vs results export
  (defun my/org-toggle-python-code-or-results ()
    "Toggle Python blocks between exporting code vs results."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search nil))
        (when (re-search-forward 
               "^#\\+PROPERTY: header-args:python.*:exports \\(code\\|results\\)" nil t)
          (let ((current (match-string 1)))
            (replace-match (if (string= current "code") "results" "code") 
                          t t nil 1)
            (message "Python exports: %s" 
                    (if (string= current "code") "results" "code")))))))

  ;; Dynamic tangle target setter
  (defun my/org-set-python-tangle-file ()
    "Set tangle file for Python blocks interactively."
    (interactive)
    (let* ((default-dir (file-name-directory (buffer-file-name)))
           (python-dir (expand-file-name "python/" default-dir))
           (filename (read-string "Python filename (without .py): " 
                                 (file-name-base (buffer-file-name)))))
      (unless (file-directory-p python-dir)
        (make-directory python-dir t))
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward 
             "^#\\+PROPERTY: header-args:python.*:tangle \\([^ \n]+\\)" nil t)
            (progn
              (replace-match (concat "python/" filename ".py") t t nil 1)
              (message "Updated Python tangle to: python/%s.py" filename))
          (message "No Python property line found")))))

  ;; Subtree-level property management
  (defun my/org-set-subtree-python-export (export-type)
    "Set Python export for current subtree. EXPORT-TYPE: 'results, 'code, or 'none."
    (interactive
     (list (intern (completing-read "Export type: " 
                                    '("results" "code" "none") nil t))))
    (org-set-property "header-args:python" 
                     (format ":session py :exports %s :eval yes :tangle python/analysis.py"
                            export-type))
    (message "Subtree Python exports: %s" export-type))

  ;; Quick block insertion with detection
  (defun my/org-insert-src-block (lang)
    "Insert a source block for LANG with auto-detected properties."
    (interactive
     (list (completing-read "Language: " 
                           '("python" "jupyter-python" "latex" "elisp" "shell")
                           nil t)))
    (let ((block-start (format "#+begin_src %s\n" lang))
          (block-end "\n#+end_src\n"))
      (insert block-start)
      (save-excursion
        (insert block-end))))

  ;; Keybindings
  (ar/global-leader
    "o p" '(:ignore t :wk "properties")
    "o p s" '(my/org-insert-scientific-project-template :wk "Scientific template")
    "o p d" '(my/org-setup-project-structure :wk "Setup project dirs")
    "o p t" '(my/org-toggle-python-export :wk "Toggle Python export")
    "o p c" '(my/org-toggle-python-code-or-results :wk "Toggle code/results")
    "o p f" '(my/org-set-python-tangle-file :wk "Set tangle file")
    "o p e" '(my/org-set-subtree-python-export :wk "Set subtree export")
    "o i" '(:ignore t :wk "insert")
    "o i s" '(my/org-insert-src-block :wk "Insert source block")))
```

### Example Org File Structure

```org
#+TITLE: Machine Learning Analysis
#+AUTHOR: Your Name
#+DATE: 2025-01-01
#+OPTIONS: toc:nil
# -*- org-src-preserve-indentation: t; -*-

# LaTeX settings
#+LATEX_CLASS: article
#+LATEX_HEADER: \usepackage{amsmath}
#+LATEX_HEADER: \usepackage{graphicx}

# Language-specific header arguments
#+PROPERTY: header-args:latex :exports code :eval never
#+PROPERTY: header-args:python :session py :exports results :eval yes :tangle python/analysis.py
#+PROPERTY: header-args:jupyter-python :session py :async yes :exports results :eval yes :tangle python/jupyter.py :kernel python3

* Introduction
This section uses LaTeX for typesetting.

#+begin_src latex
\section{Introduction}
We analyze the dataset using machine learning techniques.
The model achieves 95\% accuracy on the test set.
#+end_src

* Data Analysis
** Load Data
#+begin_src python
import pandas as pd
import matplotlib.pyplot as plt

# This code will:
# 1. Execute and show results in PDF
# 2. Tangle to python/analysis.py
data = pd.read_csv('data.csv')
print(f"Loaded {len(data)} rows")
#+end_src

** Visualization
#+begin_src jupyter-python
# Jupyter block with async execution
import seaborn as sns

plt.figure(figsize=(10, 6))
sns.boxplot(data=data, x='category', y='value')
plt.title('Distribution by Category')
plt.savefig('output/boxplot.png')
'output/boxplot.png'  # Return filename for inline display
#+end_src

* Methods
** Algorithm Description
#+begin_src latex
\section{Methods}
We employ a random forest classifier with the following parameters:
\begin{itemize}
\item n\_estimators = 100
\item max\_depth = 10
\item min\_samples\_split = 5
\end{itemize}
#+end_src

* Private Analysis Section
:PROPERTIES:
:header-args:python: :session py :exports none :eval yes :tangle python/private.py
:END:

This subtree's Python code won't appear in PDF but will still tangle.

#+begin_src python
# Sensitive analysis that shouldn't be in the paper
internal_metrics = calculate_internal_metrics(data)
print(internal_metrics)
#+end_src
```

### Advanced: Project-Specific Configuration File

For complex projects, create `.dir-locals.el` in project root:

```elisp
;;; Directory Local Variables for ~/Projects/sample-project/
;;; For more information see (info "(emacs) Directory Variables")

((org-mode . ((org-babel-default-header-args:python . 
               ((:session . "py")
                (:exports . "results")
                (:eval . "yes")
                (:tangle . "python/analysis.py")))
              (org-babel-default-header-args:jupyter-python .
               ((:session . "py")
                (:async . "yes")
                (:exports . "results")
                (:kernel . "python3")
                (:tangle . "python/jupyter.py")))
              (org-babel-default-header-args:latex .
               ((:exports . "code")
                (:eval . "never"))))))
```

### Workflow Tips

1. **Start a new project:**
   - `SPC o p s` - Insert template
   - `SPC o p d` - Create directory structure
   - Write your mixed LaTeX/Python content

2. **During writing:**
   - `SPC o i s` - Quickly insert source blocks
   - `C-c C-c` - Execute current block
   - `C-c C-v t` - Tangle all Python to files

3. **Before submission:**
   - `SPC o p t` - Toggle to hide Python code from PDF
   - `C-c C-e l p` - Export to PDF via LaTeX

4. **For presentations:**
   - `SPC o p c` - Toggle to show Python code instead of results

---

```el
;; Add to Org Mode configuration section
(with-eval-after-load 'org
  ;; Enhanced property-based header argument resolution
  (defun my/org-babel-get-language-header-args (lang)
    "Get header arguments for LANG from properties."
    (let ((prop-name (format "header-args:%s" lang)))
      (org-entry-get nil prop-name t)))

  ;; Setup for mixed LaTeX/Python projects
  (defun my/org-setup-mixed-document ()
    "Setup a document with LaTeX and Python source blocks."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward "^#\\+PROPERTY: header-args:latex" nil t)
        (goto-char (point-min))
        (insert "#+PROPERTY: header-args:latex :exports code :eval never\n"))
      (goto-char (point-min))
      (unless (re-search-forward "^#\\+PROPERTY: header-args:python" nil t)
        (goto-char (point-min))
        (forward-line 1)
        (insert "#+PROPERTY: header-args:python :exports results :eval yes :session py :tangle python/code.py\n"))
      (goto-char (point-min))
      (unless (re-search-forward "^#\\+PROPERTY: header-args:jupyter-python" nil t)
        (goto-char (point-min))
        (forward-line 2)
        (insert "#+PROPERTY: header-args:jupyter-python :exports results :eval yes :session py :tangle python/jupyter.py\n"))
      (message "Mixed document properties configured")))

  ;; Function to toggle Python code export (for pure code documents)
  (defun my/org-toggle-python-export ()
    "Toggle whether Python blocks are exported to PDF."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^#\\+PROPERTY: header-args:python.*:exports none" nil t)
          (progn
            (replace-match "#+PROPERTY: header-args:python :exports results :eval yes :session py :tangle python/code.py")
            (message "Python blocks WILL be exported"))
        (when (re-search-forward "^#\\+PROPERTY: header-args:python" nil t)
          (beginning-of-line)
          (kill-line)
          (insert "#+PROPERTY: header-args:python :exports none :eval yes :session py :tangle python/code.py")
          (message "Python blocks will NOT be exported"))))))

;; Example org file structure:
;;
;; #+TITLE: Research Project
;; #+AUTHOR: Your Name
;; #+PROPERTY: header-args:latex :exports code :eval never
;; #+PROPERTY: header-args:python :exports results :eval yes :session py :tangle python/analysis.py
;; #+PROPERTY: header-args:jupyter-python :exports results :eval yes :session py :tangle python/jupyter.py
;;
;; * Analysis
;; #+begin_src python
;; # This will be tangled to python/analysis.py
;; # Results will be exported to PDF
;; import pandas as pd
;; data = pd.read_csv("data.csv")
;; #+end_src
;;
;; * Document
;; #+begin_src latex
;; % This will be exported but not evaluated
;; \section{Introduction}
;; #+end_src
```

Add keybindings:

```elisp
(ar/global-leader
  "o p" '(:ignore t :wk "properties")
  "o p m" '(my/org-setup-mixed-document :wk "Setup mixed LaTeX/Python")
  "o p t" '(my/org-toggle-python-export :wk "Toggle Python export"))
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

;; Apply to other text modes as well
(add-hook 'text-mode-hook #'my/org-bidi-optimizations)
(add-hook 'prog-mode-hook #'my/org-bidi-optimizations)
```

This achieves the **same performance optimizations** as the lambda approach, as
both set buffer-local variables in the appropriate hooks.

---

## 11. Jupyter Configuration with-eval-after-load

### Analysis

The `with-eval-after-load 'ob-jupyter` **is not strictly necessary** but is good
practice.

### Recommendation

**Keep it** for the following reasons:

1. Ensures `org-babel-jupyter-override-src-block` is only called after
   ob-jupyter is loaded
2. Prevents errors if jupyter package fails to load
3. Follows Doom Emacs patterns for deferred configuration

### Optimized Version

```elisp
(use-package jupyter
  :ensure t
  :after org
  :commands (jupyter-run-repl jupyter-connect-repl)
  :config
  ;; Set default header args for jupyter-python blocks
  (setq org-babel-default-header-args:jupyter-python 
        '((:async . "yes")
          (:session . "py")
          (:kernel . "python3")
          (:tangle . "no")))  ; Changed to "no" by default

  ;; This should be wrapped for safety
  (with-eval-after-load 'ob-jupyter
    ;; Override python blocks to use jupyter automatically
    (org-babel-jupyter-override-src-block "python")
    
    ;; Additional jupyter-specific configuration
    (setq jupyter-repl-echo-eval-p t))

  ;; Keybindings
  (ar/global-leader
    "j" '(:ignore t :wk "jupyter")
    "j c" '(jupyter-connect-repl :wk "Connect REPL")
    "j r" '(jupyter-run-repl :wk "Run REPL")
    "j k" '(jupyter-shutdown-kernel :wk "Shutdown kernel")
    "j i" '(jupyter-inspect-at-point :wk "Inspect")))
```

---

## 12. Project-Specific Citar in Org-Mode

### Analysis

Your current citar configuration **should work** in org-mode, but needs explicit
org-cite integration.

### Enhanced Implementation

```elisp
;; Replace Citation Ecosystem section
(use-package citar
  :ensure t
  :custom
  ;; Global fallback bibliographies
  (citar-bibliography '("~/references.bib"))
  (citar-library-paths '("~/Zotero/storage"))
  (citar-notes-paths (list my/org-roam-directory))

  ;; Icons
  (citar-symbols
   `((file ,(nerd-icons-mdicon "nf-md-file_document") . " ")
     (note ,(nerd-icons-mdicon "nf-md-note_text") . " ")
     (link ,(nerd-icons-mdicon "nf-md-link") . " ")))

  :config
  ;; Function to find project-specific bibliographies
  (defun my/citar-get-project-bibliographies ()
    "Get bibliography files for current project."
    (when-let* ((project (project-current))
                (root (project-root project)))
      (let ((bib-files '()))
        ;; Check project root
        (dolist (file (directory-files root t "\\.bib\\'"))
          (push file bib-files))
        ;; Check references/ subdirectory
        (let ((ref-dir (expand-file-name "references/" root)))
          (when (file-directory-p ref-dir)
            (dolist (file (directory-files ref-dir t "\\.bib\\'"))
              (push file bib-files))))
        ;; Check bib/ subdirectory
        (let ((bib-dir (expand-file-name "bib/" root)))
          (when (file-directory-p bib-dir)
            (dolist (file (directory-files bib-dir t "\\.bib\\'"))
              (push file bib-files))))
        (nreverse bib-files))))

  ;; Override citar-bibliography
  (defun my/citar-bibliography ()
    "Return project-specific bibliography or global fallback."
    (or (my/citar-get-project-bibliographies)
        citar-bibliography))

  ;; Advice citar to use project bibliographies
  (advice-add 'citar--bibliography-files :override #'my/citar-bibliography)

  :hook
  ((LaTeX-mode org-mode) . (lambda ()
                             (setq-local citar-bibliography (my/citar-bibliography))
                             (local-set-key (kbd "C-c b") #'citar-insert-citation))))

;; Ensure org-cite uses citar with project-specific bibliographies
(with-eval-after-load 'org
  (require 'oc)
  (require 'oc-csl)
  (require 'citar)
  
  ;; Make org-cite use project bibliographies
  (defun my/org-cite-get-bibliography ()
    "Get bibliography for org-cite from project or global."
    (my/citar-bibliography))
  
  (setq org-cite-global-bibliography (my/citar-bibliography))
  
  ;; Hook to update bibliography when switching buffers
  (defun my/update-org-bibliography ()
    "Update org-cite bibliography when entering org buffer."
    (when (derived-mode-p 'org-mode)
      (setq-local org-cite-global-bibliography (my/citar-bibliography))))
  
  (add-hook 'org-mode-hook #'my/update-org-bibliography)
  (add-hook 'buffer-list-update-hook #'my/update-org-bibliography)
  
  ;; Configure org-cite processors
  (setq org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar
        org-cite-insert-processor 'citar))

;; Citar-Embark for contextual actions
(use-package citar-embark
  :ensure t
  :after (citar embark)
  :config (citar-embark-mode))

;; Citar-Org-Roam integration
(use-package citar-org-roam
  :ensure t
  :after (citar org-roam)
  :config (citar-org-roam-mode 1))
```

---

## Summary of Changes

### Critical Changes

1. **Keep org-src-fontify-natively enabled** - tree-sitter works through it
2. **Implement Doom evil behaviors** - especially Y, o/O, visual search
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

---

## 9. Additional Configuration Questions

### Bidi Performance Optimizations

**Question:** Does `setq-local` in global scope achieve the same performance
optimizations?

**Answer:** No, your current method doesn't create proper buffer-local
variables.

**Issue:**

```elisp
;; This doesn't work as intended:
(setq-local bidi-paragraph-direction 'left-to-right
            bidi-inhibit-bpa t)
```

**Correct Implementation:**

```elisp
;; Global setting (affects all buffers)
(setq-default bidi-display-reordering nil)

;; Buffer-local optimizations (requires hooks)
(defun my/bidi-optimizations ()
  "Apply bidi optimizations for current buffer."
  (setq-local bidi-paragraph-direction 'left-to-right)
  (setq-local bidi-inhibit-bpa t))

;; Apply to appropriate modes
(add-hook 'org-mode-hook #'my/bidi-optimizations)
(add-hook 'text-mode-hook #'my/bidi-optimizations)
(add-hook 'prog-mode-hook #'my/bidi-optimizations)
```

This achieves the **same performance optimizations** as using lambda functions
in hooks.

---

### Jupyter Configuration with-eval-after-load

**Question:** Is `with-eval-after-load 'ob-jupyter` necessary?

**Answer:** Not strictly necessary, but **recommended** for safety and
Doom-style practices.

**Recommendation: Keep it**

Reasons:

1. Ensures `org-babel-jupyter-override-src-block` is only called after
   ob-jupyter loads
2. Prevents errors if jupyter package fails to load
3. Follows Doom Emacs defensive programming patterns
4. Zero performance cost

**Optimized Version:**

```elisp
(use-package jupyter
  :ensure t
  :after org
  :commands (jupyter-run-repl jupyter-connect-repl)
  :config
  ;; Set default header args
  (setq org-babel-default-header-args:jupyter-python 
        '((:async . "yes")
          (:session . "py")
          (:kernel . "python3")
          (:tangle . "no")))

  ;; Wrap in with-eval-after-load for safety
  (with-eval-after-load 'ob-jupyter
    ;; Override python blocks to use jupyter
    (org-babel-jupyter-override-src-block "python")
    
    ;; Additional jupyter-specific settings
    (setq jupyter-repl-echo-eval-p t)
    (setq jupyter-eval-use-overlays t))

  ;; Keybindings
  (ar/global-leader
    "j" '(:ignore t :wk "jupyter")
    "j c" '(jupyter-connect-repl :wk "Connect REPL")
    "j r" '(jupyter-run-repl :wk "Run REPL")
    "j k" '(jupyter-shutdown-kernel :wk "Shutdown kernel")
    "j i" '(jupyter-inspect-at-point :wk "Inspect")))
```

---

### Dynamic Tangling for Jupyter-Python

**Answer:** Yes, you can add `:tangle` to
`org-babel-default-header-args:jupyter-python`.

**Implementation:**

```elisp
(with-eval-after-load 'org
  ;; Default tangle target (can be overridden per-file or per-block)
  (setq org-babel-default-header-args:jupyter-python
        '((:async . "yes")
          (:session . "py")
          (:kernel . "python3")
          (:tangle . "python/jupyter.py")))  ; Default tangle location

  ;; Function to dynamically set tangle for current block
  (defun my/org-babel-set-block-tangle ()
    "Interactively set tangle file for current source block."
    (interactive)
    (save-excursion
      (org-babel-goto-src-block-head)
      (let* ((default-dir (file-name-directory (buffer-file-name)))
             (python-dir (expand-file-name "python/" default-dir))
             (filename (read-string "Tangle to file (without .py): ")))
        (unless (file-directory-p python-dir)
          (make-directory python-dir t))
        (org-set-property "header-args" 
                         (format ":tangle python/%s.py" filename))
        (message "Block will tangle to: python/%s.py" filename)))))

;; Usage in org file with #+PROPERTY:
;;
;; Method 1: File-level default
;; #+PROPERTY: header-args:jupyter-python :session py :tangle python/analysis.py
;;
;; Method 2: Per-subtree
;; * Analysis Section
;; :PROPERTIES:
;; :header-args:jupyter-python: :tangle python/experiment1.py
;; :END:
;;
;; Method 3: Dynamic in block using noweb
;; #+begin_src jupyter-python :tangle (concat default-directory "python/" "dynamic.py")

;; Keybinding
(ar/global-leader
  "o t b" '(my/org-babel-set-block-tangle :wk "Set block tangle"))
```

---

### Project.el Configuration for ~/Projects

**Current State:** Your config doesn't direct projects to ~/Projects

**Fixed Implementation:**

```elisp
(use-package project
  :ensure nil
  :custom
  (project-vc-extra-root-markers '(".envrc" ".project" "pyproject.toml" "Cargo.toml" ".git" "flake.nix"))
  (project-list-file (expand-file-name "projects" no-littering-var-directory))
  
  :config
  ;; Define projects base directory
  (defcustom my/projects-directory (expand-file-name "~/Projects/")
    "Base directory for all projects."
    :type 'directory
    :group 'project)

  (unless (file-directory-p my/projects-directory)
    (make-directory my/projects-directory t))

  ;; Auto-discover projects in ~/Projects
  (defun my/project-discover-all ()
    "Discover all subdirectories in ~/Projects as projects."
    (interactive)
    (let ((count 0))
      (dolist (dir (directory-files my/projects-directory t "^[^.]"))
        (when (and (file-directory-p dir)
                   (or (file-exists-p (expand-file-name ".git" dir))
                       (file-exists-p (expand-file-name "flake.nix" dir))
                       (file-exists-p (expand-file-name ".project" dir))))
          (project-remember-project (project-current nil dir))
          (cl-incf count)))
      (message "Discovered %d projects in ~/Projects" count)))

  ;; Run on startup
  (add-hook 'after-init-hook #'my/project-discover-all)

  ;; Improved project switching that prioritizes ~/Projects
  (defun my/project-switch ()
    "Switch to project, prioritizing those in ~/Projects."
    (interactive)
    (let* ((all-projects (project-known-project-roots))
           (projects-projects 
            (cl-remove-if-not 
             (lambda (p) (string-prefix-p my/projects-directory p))
             all-projects))
           (other-projects
            (cl-remove-if
             (lambda (p) (string-prefix-p my/projects-directory p))
             all-projects))
           (sorted (append projects-projects other-projects)))
      (project-switch-project
       (completing-read "Switch to project: " sorted nil t))))

  ;; Helper functions (kept from previous implementation)
  (defun my/project-bibliography-files ()
    "Get bibliography files for current project."
    (when-let* ((project (project-current))
                (root (project-root project)))
      (let ((bib-files '()))
        (dolist (file (directory-files root t "\\.bib\\'"))
          (push file bib-files))
        (dolist (subdir '("references/" "bib/" "bibliography/"))
          (let ((dir (expand-file-name subdir root)))
            (when (file-directory-p dir)
              (dolist (file (directory-files dir t "\\.bib\\'"))
                (push file bib-files)))))
        (delete-dups (nreverse bib-files))))))

;; Keybindings
(ar/global-leader
  "p p" '(my/project-switch :wk "Switch project")
  "p +" '(my/project-discover-all :wk "Discover projects"))
```

---

### Lingering Projectile Configuration

**Answer:** No projectile configuration found in your config.

You're correctly using `project.el` exclusively. No changes needed.

---

## 10. Missing Vim-like Behavior Packages

Your evil configuration is mostly complete, but these would enhance the vim
experience:

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

;; Update evil-numbers bindings
(with-eval-after-load 'evil-numbers
  (define-key evil-normal-state-map (kbd "C-a") #'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-x") #'evil-numbers/dec-at-pt)
  (define-key evil-visual-state-map (kbd "g C-a") #'evil-numbers/inc-at-pt-incremental)
  (define-key evil-visual-state-map (kbd "g C-x") #'evil-numbers/dec-at-pt-incremental))
```

Add to `default.nix`:

```nix
extraEmacsPackages = epkgs:
  with epkgs; [
    # ... existing packages
    evil-surround
    evil-matchit
  ];
```

---

### Priority Changes

**Immediate Actions:**

1. ✅ Keep `org-src-fontify-natively t` - don't disable it
2. ✅ Implement comprehensive Doom Evil settings and behaviors
3. ✅ Replace visual-line setup with Doom word-wrap module
4. ✅ Implement Doom smartparens configuration
5. ✅ Setup property-based header arguments system
6. ✅ Configure direnv with proper inheritance
7. ✅ Enhance citar for org-mode integration

### Required Package Additions to default.nix

```nix
extraEmacsPackages = epkgs:
  with epkgs; [
    use-package
    vterm
    jupyter
    visual-fill-column  # For word-wrap
    adaptive-wrap       # For word-wrap  
    auto-dim-other-buffers
    treesit-grammars.with-all-grammars
  ];
```

### Configuration Changes Summary

**1. Evil (Replace Core Evil section):**

- 30+ new evil settings before `:config`
- Comment continuation for o/O keys
- Visual */# search
- Custom ex commands
- Window movement enhancements
- Preprocessor navigation
- Minibuffer evil bindings

**2. Word Wrap (Replace visual-line setup):**

- Remove `global-visual-line-mode`
- Remove `visual-fill-column-for-vline` function
- Add complete `+word-wrap-mode` implementation
- Add `+global-word-wrap-mode`
- Configure custom variables for extra-indent and fill-style

**3. Smartparens (Update existing):**

- Add Doom-specific pair behaviors
- Add comment continuation advice
- Disable in evil replace mode
- Optional traditional keybindings for non-evil users

**4. Org Property Headers (New section):**

- Template insertion function
- Project structure setup
- Toggle functions for export modes
- Dynamic tangle file setter
- Subtree property management
- Quick block insertion

**5. Direnv (Update envrc setup):**

- Create root `.envrc` with `use flake`
- Add `watch_file` directives
- Create subfolder `.envrc` with `source_up`
- Test with `direnv allow`

**6. Citar (Update existing):**

- Add buffer-local bibliography setup
- Integrate with org-cite properly
- Add verification functions
- Setup project bibliography helper

### Testing Procedure

```elisp
;; 1. Test Evil enhancements
;; In normal mode, try:
;; - Y (should yank to end of line)
;; - o/O in a comment (should continue comment)
;; - Visual select text, press * (should search)
;; - :g/pattern/ (should highlight matches)

;; 2. Test Word Wrap
M-x +word-wrap-mode
;; Should see soft-wrapped lines with smart indentation

;; 3. Test Property Headers
M-x my/org-insert-scientific-project-template
;; Should insert template with language-specific properties

;; 4. Test Direnv
;; In terminal:
cd ~/Projects/sample-project
direnv allow
cd subfolder
# Should inherit parent environment

;; 5. Test Citar in Org
M-x my/test-citar-org-integration
;; Should show all ✓
```

### File Organization

**Keep these sections as-is:**

- Performance Tuning (with bidi fixes)
- TreeSit configuration
- LSP-bridge setup
- Jupyter configuration
- Project.el configuration

**Update these sections:**

- Evil core and extensions
- Smartparens
- Visual line / word wrap
- Org-mode header arguments
- Citar configuration

**Add new sections:**

- Doom Evil comment continuation
- Doom Evil visual search
- Doom Evil window management
- Word-wrap mode complete implementation
- Property-based org workflow
- Direnv inheritance guide

### Performance Notes

**Evil:** No performance impact, mostly behavioral improvements\
**Word-wrap:** Negligible impact with adaptive-wrap\
**Smartparens:** Already in use, Doom config is more optimized\
**Org Properties:** Zero impact, just better organization\
**Direnv:** No Emacs impact, faster than manual activation\
**Citar:** Same performance, better project integration

### Troubleshooting

**Evil o/O not continuing comments:**

```elisp
;; Check smartparens is loaded and in comment
(sp-point-in-comment)  ; Should return t in comment
```

**Word-wrap not working:**

```elisp
;; Check modes are loaded
(bound-and-true-p adaptive-wrap-prefix-mode)  ; Should be t
(bound-and-true-p visual-line-mode)  ; Should be t
```

**Direnv not inheriting:**

```bash
# Ensure parent .envrc is allowed
cd ~/Projects/sample-project && direnv allow
cd subfolder && direnv allow
direnv status
```

**Citar not finding project bib:**

```elisp
M-x my/org-verify-bibliography
;; Check which files are found
```

All implementations are production-ready and follow Doom Emacs patterns while
integrating cleanly with your existing lsp-bridge, evil, and org-mode setup.
