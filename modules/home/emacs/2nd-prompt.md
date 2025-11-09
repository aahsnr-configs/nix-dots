# Emacs Configuration - Deep Dive Analysis

## Response to Second Prompt (Focused Implementation)

**Focus Areas**: Comprehensive Doom Evil Implementation, Word Wrap, Smartparens,
Property-Based Headers, Citar Integration, and Direnv Inheritance

---

## 1. Comprehensive Doom Emacs Evil Implementation

Based on extensive analysis of Doom's `modules/editor/evil/config.el`, here's a
complete implementation of Doom's Evil enhancements.

### Core Evil Settings (Before evil-mode loads)

```elisp
;; Replace existing evil :init section with Doom's comprehensive settings
(use-package evil
  :ensure t
  :init
  ;; === Doom Evil Settings - Set BEFORE evil loads ===
  
  ;; Core behavior
  (setq evil-want-C-g-bindings t
        evil-want-C-i-jump nil          ; Doom does this themselves
        evil-want-C-u-scroll t          ; Universal arg moved to SPC u
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
       (eq (plist-get context :type) 'line-comment)))

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

## 2. Comprehensive Doom Word Wrap Implementation

Doom's word-wrap module provides intelligent soft-wrapping with language-aware
indentation using `adaptive-wrap` and `visual-fill-column`.

### Installation Requirements

Add to `default.nix`:

```nix
extraEmacsPackages = epkgs:
  with epkgs; [
    # ... existing packages
    adaptive-wrap
    visual-fill-column
  ];
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
;; Replace your existing visual-line-mode setup with:
(defun visual-fill-column-for-vline ()
  "Deprecated - use +word-wrap-mode instead."
  (message "Use +word-wrap-mode for better word wrapping"))

;; Remove old hooks
(remove-hook 'after-init-hook #'global-visual-line-mode)
(remove-hook 'visual-line-mode-hook #'visual-fill-column-for-vline)

;; Add new setup
(add-hook 'after-init-hook #'+global-word-wrap-mode)
```

---

## 3. Doom Smartparens Implementation

Doom uses smartparens primarily for auto-pairing and provides intelligent
comment continuation.

### Core Smartparens Setup

```elisp
;; Replace your existing smartparens configuration
(use-package smartparens
  :ensure t
  :defer t
  :hook ((prog-mode text-mode) . smartparens-mode)
  :init
  ;; Load smartparens config for language-specific rules
  (with-eval-after-load 'smartparens
    (require 'smartparens-config))
  
  :config
  ;; Doom's smartparens settings
  (setq sp-show-pair-delay 0.1
        sp-show-pair-from-inside t
        sp-cancel-autoskip-on-backward-movement nil
        sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil
        sp-navigate-close-if-unbalanced t
        sp-message-width nil)
  
  ;; Smartparens interferes with evil replace mode
  (add-hook 'evil-replace-state-entry-hook #'turn-off-smartparens-mode)
  (add-hook 'evil-replace-state-exit-hook
            (lambda ()
              (when (and (bound-and-true-p smartparens-mode)
                         (not smartparens-mode))
                (turn-on-smartparens-mode))))
  
  ;; Doom's pair behaviors
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "'" nil :actions nil)
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "`" nil :actions nil)
  
  ;; Expand braces intelligently
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "(" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "[" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  
  ;; Don't do square-bracket space-expansion for lisp modes
  (sp-local-pair '(emacs-lisp-mode lisp-mode clojure-mode racket-mode hy-mode)
                 "(" nil :post-handlers '(("||\n[i]" "RET") ("| " " ")))
  (sp-local-pair '(emacs-lisp-mode lisp-mode clojure-mode racket-mode hy-mode)
                 "[" nil :post-handlers nil)
  
  ;; Reasonable default pairs for org-mode
  (sp-local-pair 'org-mode "~" "~" :unless '(sp-point-before-word-p))
  (sp-local-pair 'org-mode "=" "=" :unless '(sp-point-before-word-p))
  (sp-local-pair 'org-mode "*" "*" :unless '(sp-point-before-word-p))
  
  ;; Disable smartparens in some modes
  (dolist (mode '(erc-mode
                  gud-mode
                  inferior-emacs-lisp-mode
                  minibuffer-inactive-mode
                  debugger-mode))
    (add-to-list 'sp-ignore-modes-list mode)))

;; Doom's comment continuation advice
(defun +default--newline-indent-and-continue-comments-a (orig-fn &rest args)
  "Continue comments when pressing RET with smartparens."
  (interactive "P")
  (if (and (sp-point-in-comment)
           comment-line-break-function)
      (funcall comment-line-break-function (car args))
    (apply orig-fn args)))

(advice-add 'newline-and-indent :around #'+default--newline-indent-and-continue-comments-a)
```

### Smartparens Navigation Bindings (Optional)

These are for non-evil users. Evil users should use evil-textobj-tree-sitter
instead:

```elisp
;; Only if you want traditional Emacs s-expression navigation
(with-eval-after-load 'smartparens
  (define-key smartparens-mode-map (kbd "C-M-f") #'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-b") #'sp-backward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-a") #'sp-beginning-of-sexp)
  (define-key smartparens-mode-map (kbd "C-M-e") #'sp-end-of-sexp)
  (define-key smartparens-mode-map (kbd "C-M-n") #'sp-next-sexp)
  (define-key smartparens-mode-map (kbd "C-M-p") #'sp-previous-sexp)
  (define-key smartparens-mode-map (kbd "C-M-k") #'sp-kill-sexp)
  (define-key smartparens-mode-map (kbd "C-M-t") #'sp-transpose-sexp)
  (define-key smartparens-mode-map (kbd "C-M-w") #'sp-copy-sexp)
  (define-key smartparens-mode-map (kbd "M-<delete>") #'sp-unwrap-sexp)
  (define-key smartparens-mode-map (kbd "M-<backspace>") #'sp-backward-unwrap-sexp))
```

---

## 4. Sophisticated Property-Based Header Arguments for Mixed LaTeX/Python/Jupyter Projects

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

## 5. Direnv with Flake Inheritance for Subdirectories

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

**Subfolder `.envrc` (python-analysis/.envrc, latex-report/.envrc, etc.):**

```bash
# Method A: Explicit parent reference (recommended)
source_up

# Method B: Use parent's flake directly
# use flake ..

# Method C: Source parent and add local customizations
# source_up
# export LOCAL_VAR="subfolder-specific-value"
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

### Method 3: Complete Example for Your Use Case

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
# Solution: Use nix-direnv for caching (already in your envrc setup)
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

## 6. Enhanced Project-Specific Citar for Org-Mode

Complete integration ensuring citar works seamlessly in both LaTeX and org-mode
with project-specific bibliographies.

### Complete Citar Configuration

```elisp
(use-package citar
  :ensure t
  :custom
  ;; Global fallback bibliographies
  (citar-bibliography '("~/references.bib"))
  (citar-library-paths '("~/Zotero/storage"))
  (citar-notes-paths (list my/org-roam-directory))

  ;; Appearance
  (citar-symbols
   `((file ,(nerd-icons-mdicon "nf-md-file_document") . " ")
     (note ,(nerd-icons-mdicon "nf-md-note_text") . " ")
     (link ,(nerd-icons-mdicon "nf-md-link") . " ")))
  
  ;; Display template
  (citar-display-transform-functions
   '((t . citar-display-transform-emojis)))

  :config
  ;; Project bibliography detection
  (defun my/citar-get-project-bibliographies ()
    "Get bibliography files for current project.
Searches in: project-root/, project-root/references/, project-root/bib/"
    (when-let* ((project (project-current))
                (root (project-root project)))
      (let ((bib-files '())
            (search-dirs (list root
                              (expand-file-name "references/" root)
                              (expand-file-name "bib/" root)
                              (expand-file-name "bibliography/" root))))
        (dolist (dir search-dirs)
          (when (file-directory-p dir)
            (dolist (file (directory-files dir t "\\.bib\\'"))
              (push file bib-files))))
        (delete-dups (nreverse bib-files)))))

  ;; Dynamic bibliography function
  (defun my/citar-bibliography ()
    "Return project-specific bibliography or global fallback."
    (or (my/citar-get-project-bibliographies)
        citar-bibliography))

  ;; Override citar's bibliography files function
  (advice-add 'citar--bibliography-files :override #'my/citar-bibliography)

  ;; Set buffer-local bibliography on file open
  (defun my/citar-set-local-bibliography ()
    "Set buffer-local bibliography when opening org/LaTeX files."
    (when (and (buffer-file-name)
               (or (derived-mode-p 'org-mode)
                   (derived-mode-p 'latex-mode)
                   (derived-mode-p 'LaTeX-mode)))
      (setq-local citar-bibliography (my/citar-bibliography))
      ;; Also set for org-cite
      (when (derived-mode-p 'org-mode)
        (setq-local org-cite-global-bibliography (my/citar-bibliography)))))

  ;; Apply to various hooks
  (add-hook 'org-mode-hook #'my/citar-set-local-bibliography)
  (add-hook 'LaTeX-mode-hook #'my/citar-set-local-bibliography)
  (add-hook 'latex-mode-hook #'my/citar-set-local-bibliography)
  
  ;; Update when switching buffers
  (add-hook 'buffer-list-update-hook
            (lambda ()
              (when (and (buffer-file-name)
                         (or (derived-mode-p 'org-mode)
                             (derived-mode-p 'LaTeX-mode)))
                (my/citar-set-local-bibliography))))

  ;; Keybindings for both modes
  :bind
  (:map org-mode-map
   ("C-c b" . citar-insert-citation)
   ("C-c o" . citar-open))
  (:map LaTeX-mode-map
   ("C-c b" . citar-insert-citation)
   ("C-c o" . citar-open)))

;; Org-cite integration
(with-eval-after-load 'oc
  (require 'citar)
  
  ;; Use citar for all org-cite operations
  (setq org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar)
  
  ;; Ensure org-cite uses project bibliographies
  (defun my/org-cite-list-bibliography-files ()
    "List bibliography files for org-cite."
    (my/citar-bibliography))
  
  (advice-add 'org-cite-list-bibliography-files 
              :override #'my/org-cite-list-bibliography-files))

;; Citar-Embark integration
(use-package citar-embark
  :ensure t
  :after (citar embark)
  :config
  (citar-embark-mode)
  
  ;; Add embark actions for citations
  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist '(citar-reference . citar-embark-map))))

;; Citar-Org-Roam integration for literature notes
(use-package citar-org-roam
  :ensure t
  :after (citar org-roam)
  :config
  (citar-org-roam-mode 1)
  (setq citar-org-roam-subdir "literature"))

;; RefTeX for cross-references (not citations)
(use-package reftex
  :after tex
  :hook (LaTeX-mode . reftex-mode)
  :config
  (setq reftex-plug-into-AUCTeX t)
  ;; Let citar handle citations, reftex handles cross-refs
  (setq reftex-default-bibliography nil))
```

### Org-Mode Specific Enhancements

```elisp
(with-eval-after-load 'org
  ;; Insert citation function for org-mode
  (defun my/org-insert-citation ()
    "Insert citation using citar in org-mode."
    (interactive)
    (if (fboundp 'citar-insert-citation)
        (citar-insert-citation)
      (message "Citar not available")))

  ;; Open citation at point
  (defun my/org-open-citation-at-point ()
    "Open citation at point using citar."
    (interactive)
    (if (and (fboundp 'citar-open)
             (org-in-regexp org-link-bracket-re))
        (citar-open)
      (message "No citation at point")))

  ;; Verify bibliography files exist
  (defun my/org-verify-bibliography ()
    "Check if bibliography files exist and are readable."
    (interactive)
    (let ((bib-files (my/citar-bibliography)))
      (if bib-files
          (progn
            (message "Found %d bibliography file(s):" (length bib-files))
            (dolist (file bib-files)
              (message "  %s %s" 
                      (if (file-readable-p file) "✓" "✗")
                      file)))
        (message "No bibliography files found. Using global fallback."))))

  ;; Org-mode citation keybindings
  (define-key org-mode-map (kbd "C-c C-b") #'my/org-insert-citation)
  (define-key org-mode-map (kbd "C-c C-o") #'my/org-open-citation-at-point))
```

### Project Setup Helper

```elisp
(defun my/setup-project-bibliography ()
  "Setup bibliography for current project."
  (interactive)
  (if-let* ((project (project-current))
            (root (project-root project)))
      (let* ((bib-dir (expand-file-name "references/" root))
             (bib-file (expand-file-name "references.bib" bib-dir)))
        (unless (file-directory-p bib-dir)
          (make-directory bib-dir t))
        (unless (file-exists-p bib-file)
          (with-temp-file bib-file
            (insert "% Bibliography for " (file-name-nondirectory (directory-file-name root)) "\n")
            (insert "% Created: " (format-time-string "%Y-%m-%d") "\n\n")))
        (message "Bibliography setup complete: %s" bib-file)
        bib-file)
    (message "Not in a project")))

;; Add to keybindings
(ar/global-leader
  "p B" '(my/setup-project-bibliography :wk "Setup project bibliography")
  "p v" '(my/org-verify-bibliography :wk "Verify bibliography"))
```

### Testing and Verification

```elisp
;; Test function
(defun my/test-citar-org-integration ()
  "Test citar integration in org-mode."
  (interactive)
  (let ((test-results '()))
    ;; Test 1: Citar loaded
    (push (cons "Citar loaded" (featurep 'citar)) test-results)
    
    ;; Test 2: Bibliography files found
    (push (cons "Bibliography files" 
                (> (length (my/citar-bibliography)) 0))
          test-results)
    
    ;; Test 3: Org-cite configured
    (push (cons "Org-cite configured"
                (and (boundp 'org-cite-insert-processor)
                     (eq org-cite-insert-processor 'citar)))
          test-results)
    
    ;; Test 4: Buffer-local bibliography set
    (push (cons "Buffer-local bib"
                (local-variable-p 'citar-bibliography))
          test-results)
    
    ;; Display results
    (with-current-buffer (get-buffer-create "*Citar Test Results*")
      (erase-buffer)
      (insert "Citar Org-Mode Integration Test Results\n")
      (insert "========================================\n\n")
      (dolist (result test-results)
        (insert (format "%s %s\n"
                       (if (cdr result) "✓" "✗")
                       (car result))))
      (insert "\nBibliography files:\n")
      (dolist (file (my/citar-bibliography))
        (insert (format "  - %s\n" file)))
      (display-buffer (current-buffer)))))
```

### Usage Examples

**In org-mode:**

```org
#+TITLE: Research Paper
#+BIBLIOGRAPHY: references/refs.bib

* Introduction
According to recent studies [cite:@smith2023], we can see...

# Insert citation: C-c b
# Open citation: C-c o
# Or use: C-c C-b and C-c C-o
```

**Verify setup:**

1. Open an org file in a project
2. Run: `M-x my/test-citar-org-integration`
3. Check all items show ✓

**Create project bibliography:**

1. Navigate to project
2. Run: `SPC p B` (my/setup-project-bibliography)
3. Edit `references/references.bib`

---

## 7. Summary and Implementation Checklist

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
    evil-surround       # Additional vim-like behavior
    evil-matchit        # Additional vim-like behavior
  ];
```

### Priority Implementation Order

1. **Evil Enhancements** (Section 1)
   - Replace `:init` section with 30+ Doom settings
   - Add comment continuation (o/O)
   - Add visual search (*/#)
   - Add ex commands and window management

2. **Word Wrap** (Section 2)
   - Remove `global-visual-line-mode` setup
   - Add complete `+word-wrap-mode` implementation
   - Add `+global-word-wrap-mode`
   - Configure in `after-init-hook`

3. **Smartparens** (Section 3)
   - Update existing smartparens config
   - Add Doom pair behaviors
   - Add comment continuation advice
   - Add evil replace mode integration

4. **Property-Based Org Headers** (Section 4)
   - Add template insertion functions
   - Add toggle functions
   - Add project structure helpers
   - Add keybindings under `SPC o p`

5. **Direnv Setup** (Section 5)
   - Create root `.envrc` with `use flake`
   - Add `watch_file` directives
   - Create subfolder `.envrc` files with `source_up`
   - Run `direnv allow` in root and subfolders

6. **Citar Enhancement** (Section 6)
   - Update citar configuration
   - Add buffer-local bibliography setup
   - Add org-cite integration
   - Add verification functions

### Testing Procedure

```bash
# Terminal tests
cd ~/Projects/sample-project
direnv allow
echo $PROJECT_ROOT  # Should show project path
cd subfolder
echo $PROJECT_ROOT  # Should still show parent project path
```

```elisp
;; Emacs tests

;; 1. Test Evil enhancements
;; - Press Y in normal mode (should yank to EOL)
;; - Press o in a comment (should continue comment)
;; - Visual select text, press * (should search)

;; 2. Test Word Wrap
M-x +word-wrap-mode
;; Should see soft-wrapped lines with smart indentation

;; 3. Test Org Properties
M-x my/org-insert-scientific-project-template
;; Should insert complete template

;; 4. Test Citar
M-x my/test-citar-org-
```
