I'll analyze your configuration and address each issue systematically. Let me
search for relevant information first.Now I'll provide comprehensive solutions
for all your issues:

## 1. Duplicate/Overlapping Configurations

Here are the changes needed to remove duplicates and conflicts:

```emacs-lisp
;; In ** Org Mode Integration (Line ~2770)
;; REMOVE duplicate org-cite configuration that's already in Citar section
;; Delete these lines as they're redundant:
(with-eval-after-load 'org
  (setq org-cite-global-bibliography (my/citar-bibliography)
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar
        org-cite-insert-processor 'citar))

;; In ** Smartparens (around line 1790)
;; REMOVE duplicate evil replace mode hooks - they appear twice
;; Keep only the version in :config section, delete from with-eval-after-load

;; In ** Core Configuration (around line 420)
;; REMOVE duplicate visual-line-mode setup
;; The Lines Behaviour section already handles this
;; Delete this line from add-hook at line ~520:
(add-hook 'after-init-hook #'global-visual-line-mode)
```

## 2. Property-Based Header Arguments System

This is actually already supported by Org! Here's an enhanced implementation:

```emacs-lisp
;; Add this NEW section after * Org Mode but before * Jupyter Notebooks

* Property-Based Header Arguments for Projects
My org files will:
1. Write scientific documents in LaTeX blocks (exported to PDF)
2. Perform analysis in Python/Jupyter blocks (tangled to =.py= files)
3. Optionally include or exclude Python output in PDF exports
4. Automatically apply language-specific header args via =#+PROPERTY=

#+begin_src emacs-lisp
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

  ;; Improved template with proper #+PROPERTY syntax
  (defun my/org-insert-scientific-project-template ()
    "Insert header template for scientific org document."
    (interactive)
    (goto-char (point-min))
    (insert "#+TITLE: Scientific Analysis\n")
    (insert "#+AUTHOR: " user-full-name "\n")
    (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n")
    (insert "#+OPTIONS: toc:nil\n")
    (insert "#+STARTUP: overview indent\n")
    (insert "# -*- org-src-preserve-indentation: t; -*-\n\n")
    
    (insert "# LaTeX configuration for export\n")
    (insert "#+LATEX_CLASS: article\n")
    (insert "#+LATEX_HEADER: \\usepackage{amsmath}\n")
    (insert "#+LATEX_HEADER: \\usepackage{graphicx}\n\n")
    
    (insert "# Language-specific header arguments\n")
    (insert "# LaTeX: never evaluate, export code for PDF\n")
    (insert "#+PROPERTY: header-args:latex :exports code :eval never\n\n")
    
    (insert "# Python: export results, tangle to python/, evaluate on demand\n")
    (insert "#+PROPERTY: header-args:python :session py :exports results :eval never-export :tangle python/analysis.py\n\n")
    
    (insert "# Jupyter-Python: same as Python but async\n")
    (insert "#+PROPERTY: header-args:jupyter-python :session py :async yes :exports results :eval never-export :tangle python/jupyter.py :kernel python3\n\n")
    
    (insert "# To toggle Python export, use my/org-toggle-python-export\n")
    (insert "# To change tangle file, use my/org-set-python-tangle-file\n\n")
    (message "Inserted scientific project template"))

  ;; Toggle Python export: results vs none
  (defun my/org-toggle-python-export ()
    "Toggle Python/Jupyter blocks between exporting results vs. none."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let ((changed 0))
        ;; Toggle Python
        (when (re-search-forward 
               "^#\\+PROPERTY: header-args:python.*:exports \\(results\\|none\\)" nil t)
          (let ((current (match-string 1)))
            (replace-match (if (string= current "results") "none" "results") 
                          t t nil 1)
            (setq changed (1+ changed))))
        ;; Toggle Jupyter-Python
        (goto-char (point-min))
        (when (re-search-forward 
               "^#\\+PROPERTY: header-args:jupyter-python.*:exports \\(results\\|none\\)" nil t)
          (let ((current (match-string 1)))
            (replace-match (if (string= current "results") "none" "results") 
                          t t nil 1)
            (setq changed (1+ changed))))
        (if (> changed 0)
            (progn
              (org-mode-restart)
              (message "Toggled %d language export setting(s). Press C-c C-c on #+PROPERTY lines to refresh." changed))
          (message "No Python/Jupyter property lines found")))))

  ;; Set tangle file for Python blocks
  (defun my/org-set-python-tangle-file ()
    "Set tangle file for Python and Jupyter-Python blocks."
    (interactive)
    (let* ((default-dir (file-name-directory (buffer-file-name)))
           (python-dir (expand-file-name "python/" default-dir))
           (filename (read-string "Python filename (without .py): " 
                                 (file-name-base (buffer-file-name)))))
      (unless (file-directory-p python-dir)
        (make-directory python-dir t))
      (save-excursion
        (goto-char (point-min))
        (let ((changed 0))
          ;; Update Python tangle
          (when (re-search-forward 
                 "^#\\+PROPERTY: header-args:python.*:tangle \\([^ \n]+\\)" nil t)
            (replace-match (concat "python/" filename ".py") t t nil 1)
            (setq changed (1+ changed)))
          ;; Update Jupyter-Python tangle
          (goto-char (point-min))
          (when (re-search-forward 
                 "^#\\+PROPERTY: header-args:jupyter-python.*:tangle \\([^ \n]+\\)" nil t)
            (replace-match (concat "python/" filename ".py") t t nil 1)
            (setq changed (1+ changed)))
          (if (> changed 0)
              (progn
                (org-mode-restart)
                (message "Updated %d tangle path(s) to python/%s.py. Press C-c C-c on #+PROPERTY lines to refresh." 
                        changed filename))
            (message "No Python/Jupyter property lines found"))))))

  ;; Set export type for current subtree
  (defun my/org-set-subtree-export (lang export-type)
    "Set export type for LANG in current subtree. EXPORT-TYPE: results, code, both, or none."
    (interactive
     (list (completing-read "Language: " '("python" "jupyter-python" "latex") nil t)
           (completing-read "Export type: " '("results" "code" "both" "none") nil t)))
    (org-set-property (format "header-args:%s" lang)
                     (format ":exports %s" export-type))
    (message "Set %s exports to %s for current subtree" lang export-type))

  ;; Quick insertion with property awareness
  (defun my/org-insert-src-block (lang)
    "Insert a source block for LANG. Properties are applied automatically."
    (interactive
     (list (completing-read "Language: " 
                           '("python" "jupyter-python" "latex" "emacs-lisp" "shell")
                           nil t)))
    (insert (format "#+begin_src %s\n\n#+end_src\n" lang))
    (forward-line -2)
    (message "Inserted %s block. Header args from #+PROPERTY will be applied automatically." lang))

  ;; Keybindings
  (ar/global-leader
    "o p" '(:ignore t :wk "properties")
    "o p s" '(my/org-insert-scientific-project-template :wk "Scientific template")
    "o p d" '(my/org-setup-project-structure :wk "Setup project dirs")
    "o p t" '(my/org-toggle-python-export :wk "Toggle Python export")
    "o p f" '(my/org-set-python-tangle-file :wk "Set tangle file")
    "o p e" '(my/org-set-subtree-export :wk "Set subtree export")
    "o i s" '(my/org-insert-src-block :wk "Insert source block")))
#+end_src
```

- [x] _Citar Integration for Org-Mode_

Add these improvements to your existing Citar configuration:

```el
;; Add to the existing (use-package citar ...) :config section

;; Ensure org-mode uses project-specific bibliographies
(with-eval-after-load 'org
  (require 'citar) 
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

;; Add project bibliography setup helper
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

4. [ ] _Fix Red Bracket Highlighting Issue_

The issue is caused by `show-paren-mode` conflicting with angle brackets. Add
this fix:

```emacs-lisp
;; Add to ** General Behaviour section (around line 500)

;; Fix angle bracket handling for show-paren-mode
(with-eval-after-load 'paren
  ;; Disable show-paren-mode's angle bracket support as it causes issues
  ;; Smartparens handles angle brackets better
  (setq show-paren-data-function #'show-paren--default)
  
  ;; Alternatively, if you want to use smartparens' highlighting instead:
  (setq show-paren-mode nil))

;; Configure smartparens to handle angle brackets properly
(with-eval-after-load 'smartparens
  ;; Disable smartparens for angle brackets in modes where they're operators
  (sp-local-pair 'prog-mode "<" nil :actions nil)
  
  ;; Re-enable only where angle brackets are actual pairs (e.g., templates)
  (sp-local-pair 'c++-mode "<" ">" 
                 :when '(sp-point-after-word-p)
                 :unless '(sp-point-before-same-p))
  
  ;; Prevent red highlighting on unmatched pairs in comments/strings
  (setq sp-show-pair-from-inside nil
        sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil))
```

5. [ ] _Debug Green Color Issue_

The green color in the screenshot appears to be from `git-gutter` or `diff-hl`.
Add this debugging helper:

```emacs-lisp
;; Add this to ** Miscellaneous section

;; Debug face at point
(defun my/what-face ()
  "Show face information at point for debugging."
  (interactive)
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face
        (message "Face: %s" face)
      (message "No face at point"))))

;; Bind to a key
(global-set-key (kbd "C-c f") #'my/what-face)

;; If the green is from git-gutter, you can disable it:
(with-eval-after-load 'git-gutter
  ;; Adjust git-gutter colors to match theme
  (set-face-background 'git-gutter:modified "#e0af68") ; yellow
  (set-face-background 'git-gutter:added "#9ece6a")    ; green
  (set-face-background 'git-gutter:deleted "#f7768e")  ; red
  
  ;; Or disable git-gutter in specific modes if it's interfering
  (add-hook 'org-mode-hook (lambda () (git-gutter-mode -1))))
```

## Summary of Changes

1. **Removed duplicates**: Org-cite config, smartparens evil hooks,
   visual-line-mode
2. **Property-based headers**: Complete system with templates and helpers
3. **Citar org integration**: Enhanced with verification and project setup
4. **Bracket highlighting fix**: Disabled angle bracket handling in
   show-paren-mode
5. **Green color debug**: Added face inspection tool

To apply these changes:

1. Add the new property-based system section
2. Apply all the fixes to existing sections
3. Restart Emacs or reload your configuration
4. Use `my/what-face` to identify the green highlighting source
5. Test with `SPC o p s` to insert a scientific project template

The system now properly supports language-specific `#+PROPERTY` lines that
automatically apply to all blocks of that language, without needing to specify
header args on each block!
