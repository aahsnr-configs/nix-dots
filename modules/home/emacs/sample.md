```org
;; ========================================
;; LaTeX Configuration with lsp-bridge
;; (Minimal setup without capf or flymake integration)
;; ========================================

;; 1. LSP-BRIDGE CORE CONFIGURATION
;; ========================================
(use-package lsp-bridge
  :config
  (global-lsp-bridge-mode)

  ;; LSP Server Configuration
  (setq lsp-bridge-tex-lsp-server "texlab")
  
  ;; Disable capf backend (performance concern)
  (setq acm-enable-capf nil)
  
  ;; Completion settings
  (setq acm-enable-doc t
        acm-enable-doc-markdown-render 'async
        acm-enable-icon t
        acm-candidate-match-function 'orderless-literal
        acm-backend-search-file-words-enable-fuzzy-match t)
  
  ;; Enable inlay hints
  (setq lsp-bridge-enable-inlay-hint t
        lsp-bridge-enable-hover-diagnostic t)
  
  ;; Disable auto-formatting (we'll handle this separately)
  (setq lsp-bridge-enable-auto-format-code nil))

;; 2. AUCTEX CONFIGURATION
;; ========================================
(use-package texeleia
  :config
  ;; For standalone `.tex` files
  (setf (alist-get 'LaTeX-mode apheleia-mode-alist)
        '(latexindent))
  
  (setf (alist-get 'latexindent apheleia-formatters)
        '("latexindent" "-g" "/dev/null"))
  
  ;; For Org mode "latex" source blocks
  (add-to-list 'apheleia-formatters
               '(latex . ("latexindent" "-g" "/dev/null")))
  
  ;; Enable apheleia in LaTeX buffers
  (add-hook 'LaTeX-mode-hook #'apheleia-mode))

;; 4. SYNTAX CHECKING VIA COMPILATION BUFFER
;; ========================================
;; Run syntax checking manually via C-c C-c ChkTeX or C-c C-c Check
;; Navigate errors with C-x ` (next-error)
;; This approach avoids flymake entirely

;; Optional: Add keybindings for quick access
(with-eval-after-load 'latex
  (define-key LaTeX-mode-map (kbd "C-c C-k") 
    (lambda () 
      (interactive)
      (TeX-command "ChkTeX" 'TeX-master-file)))
  
  (define-key LaTeX-mode-map (kbd "C-c C-l") 
    (lambda () 
      (interactive)
      (TeX-command "Check" 'TeX-master-file))))

;; 5. TEXPRESSO INTEGRATION (OPTIONAL)
;; ========================================
;; TeXpresso provides live rendering but has limitations
;; Only use if you need instant preview for specific documents
;; See: https://github.com/let-def/texpresso

(use-package texpresso
  :if (executable-find "texpresso")
  :commands (texpresso texpresso-display-output)
  :config
  ;; Set path to texpresso binary if not in PATH
  ;; (setq texpresso-binary "/path/to/texpresso")
  
  ;; Optional: Enable following cursor
  (setq texpresso-follow-cursor nil)
  
  ;; Keybindings
  (with-eval-after-load 'latex
    (define-key LaTeX-mode-map (kbd "C-c C-v") #'texpresso)
    (define-key LaTeX-mode-map (kbd "C-c C-o") #'texpresso-display-output)))

;; 6. EVIL-TEX INTEGRATION (IF USING EVIL)
;; ========================================
(use-package evil-tex
  :ensure t
  :after (tex evil)
  :defer t
  :hook (LaTeX-mode . evil-tex-mode))

;; ========================================
;; USAGE NOTES
;; ========================================
;; 
;; COMPLETION:
;; - lsp-bridge provides LSP completion from texlab
;; - AUCTeX completion is NOT integrated (to avoid performance hit)
;; - You get: LSP completions, file words, yasnippet, etc.
;; - You don't get: AUCTeX's \ref{}, \cite{} completions
;;   (but texlab provides these via LSP)
;;
;; SYNTAX CHECKING:
;; - Use C-c C-c ChkTeX RET to run chktex
;; - Use C-c C-c Check RET to run lacheck
;; - Or use custom keybindings: C-c C-k (ChkTeX), C-c C-l (lacheck)
;; - Navigate errors with C-x ` (next-error)
;; - No flymake = no in-buffer annotations, but compilation buffer works well
;;
;; FORMATTING:
;; - Apheleia provides latexindent integration
;; - Format on save or manually with M-x apheleia-format-buffer
;; - AUCTeX's built-in indentation still works with TAB
;; - Use C-M-\ to indent region
;;
;; TEXPRESSO:
;; - Optional live preview system
;; - Limitations: citations, aux files, can be flaky
;; - Best for: small documents, quick edits
;; - Not recommended as primary compilation method
;; - Use M-x texpresso to start
;; - Pair with traditional compilation for complex documents
;;
;; ALTERNATIVE APPROACH - COMBINING BOTH:
;; - Use lsp-bridge + texlab for: navigation, hover, diagnostics
;; - Use texpresso for: live preview during active editing
;; - Use tectonic/latexmk for: final compilation
;; - Use chktex via compilation buffer for: thorough style checking
;; 
;; This gives you best of all worlds without performance compromises

;; ========================================
;; RECOMMENDATION
;; ========================================
;; Based on the research:
;; 
;; 1. TeXpresso is NOT a replacement for texlab
;;    - TeXpresso: live rendering engine (like a super-fast compiler + viewer)
;;    - Texlab: LSP server (provides IDE features)
;;    - They serve different purposes and can complement each other
;;
;; 2. Optimal Setup:
;;    - lsp-bridge + texlab: For code intelligence (completion, navigation, hover)
;;    - AUCTeX: For LaTeX-specific commands and environment management
;;    - apheleia + latexindent: For formatting
;;    - chktex/lacheck via compilation buffer: For syntax checking
;;    - TeXpresso (optional): For live preview when needed
;;    - Tectonic/latexmk: For final compilation
;;
;; 3. What you LOSE without capf integration:
;;    - AUCTeX's context-aware completions (but texlab provides similar via LSP)
;;    - Seamless merging of LSP and AUCTeX candidates
;;    - What you GAIN:
;;    - Better performance (no capf overhead)
;;    - Simpler configuration
;;    - Still get excellent LSP features from texlab
;;
;; 4. TeXpresso verdict:
;;    - Great for: Quick edits, visual documents, presentations
;;    - Not great for: Complex documents with citations, as primary tool
;;    - Status: Experimental, work in progress
;;    - Recommend: Keep traditional setup, add TeXpresso as optional tool

  :ensure auctex
  :defer t
  :config
  ;; Set the default TeX engine to Tectonic
  (setq TeX-engine 'tectonic)
  (add-to-list 'TeX-engine-alist
               '(tectonic "Tectonic" "tectonic -X compile %s -o %o" 
                         "tectonic -X compile %s -o %o" 
                         "tectonic -X compile %s -o %o"))

  ;; Add ChkTeX and lacheck commands for syntax checking
  ;; These work via compilation buffer, no flymake needed
  (setq TeX-command-list
        (append TeX-command-list
                '(("Tectonic" "tectonic -X compile %s" TeX-run-command nil 
                   (latex-mode) :help "Compile with Tectonic")
                  ("Tectonic Watch" "tectonic -X watch %s" TeX-run-command nil 
                   (latex-mode) :help "Continuously compile with Tectonic")
                  ("ChkTeX" "chktex -v0 -q -I %s" TeX-run-compile nil 
                   (latex-mode) :help "Check with ChkTeX")
                  ("Check" "lacheck %s" TeX-run-compile nil 
                   (latex-mode) :help "Check with lacheck"))))

  ;; Use PDF-Tools as the default viewer and enable source correlation
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (setq TeX-source-correlate-mode t)
  (setq TeX-PDF-mode t)

  ;; Enable folding of macros and environments
  (add-hook 'LaTeX-mode-hook #'TeX-fold-mode)
  
  ;; Enable auto-indentation on newline
  (setq TeX-newline-function 'newline-and-indent)
  
  ;; Configure indentation levels
  (setq LaTeX-indent-level 2
        LaTeX-item-indent 0
        TeX-brace-indent-level 2))

```


```
** Core Backend: AUCTeX and Tectonic
This configures the foundational packages. *AUCTeX* is the primary editing environment, enhanced with *Tectonic* as the default compiler for its modern, all-in-one approach.
#+begin_src emacs-lisp
(use-package tex
  :ensure auctex
  :defer t
  :config
  ;; Set the default TeX engine to Tectonic.
  (setq TeX-engine 'tectonic)
  (add-to-list 'TeX-engine-alist
               '(tectonic "Tectonic" "tectonic -X compile %s -o %o" "tectonic -X compile %s -o %o" "tectonic -X compile %s -o %o"))

  ;; Add commands for single compilation and continuous watching.
  (setq TeX-command-list
        '(("Tectonic" "tectonic -X compile %s" TeX-run-command nil (latex-mode) :help "Compile with Tectonic")
          ("Tectonic Watch" "tectonic -X watch %s" TeX-run-command nil (latex-mode) :help "Continuously compile with Tectonic")))

  ;; Use PDF-Tools as the default viewer and enable source correlation (SyncTeX).
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (setq TeX-source-correlate-mode t)
  (setq TeX-PDF-mode t)

  ;; Enable folding of macros and environments, which is built into AUCTeX.
  (add-hook 'LaTeX-mode-hook #'TeX-fold-mode))

;; Provides evil-mode integration for AUCTeX environments.
(use-package evil-tex
  :ensure t
  :after (tex evil)
  :defer t)
#+end_src

** LSP, Completion, and Diagnostics
This section integrates modern tooling for a responsive and intelligent editing experience.
- *Eglot + Texlab:* Provides Language Server Protocol features.
- *Completion:* Merges candidates from Eglot (LSP) and AUCTeX for the most comprehensive suggestions.
- *Flymake + ChkTeX:* Offers on-the-fly syntax and style checking.
#+begin_src emacs-lisp
;; Integrate texlab with Eglot for LSP support.
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '((latex-mode tex-mode plain-tex-mode) . ("texlab"))))
;; 
;; ;; This function correctly merges AUCTeX's completion functions with the
;; ;; LSP-provided functions from Eglot. Cape and Corfu will display the merged list.
;; (defun ar/latex-completion-setup ()
;;   "Add AUCTeX completion backend to the local completion functions."
;;   (add-to-list 'completion-at-point-functions #'TeX-completion-at-point-function nil t))
;; (add-hook 'LaTeX-mode-hook #'ar/latex-completion-setup)
;; 
;; ;; Enable the built-in ChkTeX support from AUCTeX for style checking.
;; ;; This requires the `chktex` command-line tool to be installed.
;; (add-hook 'LaTeX-mode-hook
;;           (lambda ()
;;             (flymake-add-checker 'tex-chktex)))
#+end_src

** Auto-formatting: Apheleia and latexindent
This configures *apheleia* to use the *latexindent* tool for formatting, ensuring consistent and clean source code.
#+begin_src emacs-lisp
(with-eval-after-load 'apheleia
  ;; For standalone `.tex` files.
  (setf (alist-get 'LaTeX-mode apheleia-formatters)
        '("latexindent" "-g" "/dev/null"))

  ;; For Org mode "latex" source blocks.
  (add-to-list 'apheleia-formatters-alist
               '((latex . ("latexindent" "-g" "/dev/null")))))
#+end_src

** Citation Ecosystem: Citar and Zotero
This section configures a streamlined citation workflow centered around **Citar** and Zotero. It removes redundant packages for a simpler, more powerful setup.
#+begin_src emacs-lisp
;; RefTeX is still useful for non-citation references (labels, etc.).
(use-package reftex
  :after tex
  :config
  (add-hook 'LaTeX-mode-hook #'reftex-mode)
  (setq reftex-plug-into-AUCTeX t)
  ;; Let Citar handle the bibliography files.
  (setq reftex-default-bibliography '()))

;; Citar is the core of our citation workflow.
(use-package citar
  :ensure t
  :hook ((latex-mode . citar-capf-setup)
         (org-mode . citar-capf-setup))
  :custom
  ;; --- CRITICAL ---
  ;; Point this to the `.bib` file that Better BibTeX for Zotero auto-exports.
  (citar-bibliography '())
  ;; Point this to your Zotero data directory to find attached PDFs.
  (citar-library-paths '("~/Zotero/storage"))
  (citar-notes-paths (list my/org-roam-directory))
  (citar-symbols
   `((file ,(nerd-icons-mdicon "nf-md-file_document") . " ")
     (note ,(nerd-icons-mdicon "nf-md-note_text") . " ")
     (link ,(nerd-icons-mdicon "nf-md-link") . " "))))

;; Integrates Citar with Org Roam to link literature notes to citations.
(use-package citar-org-roam
  :ensure t
  :after (citar org-roam)
  :config (citar-org-roam-mode 1))

;; Provides Embark actions for Citar candidates (e.g., open PDF, open notes).
(use-package citar-embark
  :ensure t
  :after (citar embark)
  :config (citar-embark-mode))
#+end_src

** Writing UI and Editing Enhancements
This section improves the interactive writing experience with faster math input
and aesthetic ligatures.
#+begin_src emacs-lisp
(use-package math-symbol-lists :ensure t)

(use-package cdlatex
  :ensure t
  :hook (LaTeX-mode . cdlatex-mode))

(use-package laas
  :ensure t
  :hook (LaTeX-mode . laas-mode))

;; Use `prettify-symbols-mode` to render LaTeX macros as unicode characters.
(defun ar/latex-prettify-symbols-setup ()
  "Enable prettify-symbols-mode and add custom LaTeX ligatures."
  (prettify-symbols-mode 1)
  (mapc (lambda (rule) (push rule prettify-symbols-compose-rules))
        '(("\\sum" . "∑") ("\\int" . "∫") ("\\in" . "∈") ("\\forall" . "∀")
          ("\\exists" . "∃") ("\\lambda" . "λ") ("\\alpha" . "α") ("\\beta" . "β")
          ("\\gamma" . "γ") ("\\delta" . "δ") ("\\epsilon" . "ε") ("\\pi" . "π")
          ("\\rightarrow" . "→") ("\\leftarrow" . "←") ("\\Rightarrow" . "⇒")
          ("\\leq" . "≤") ("\\geq" . "≥"))))
(add-hook 'LaTeX-mode-hook #'ar/latex-prettify-symbols-setup)
#+end_src

** Org Mode Integration
#+begin_src emacs-lisp
(with-eval-after-load 'ox-latex
  ;; Configure Org's citation engine to use our Citar setup.
  (setq org-cite-global-bibliography '())
  (setq org-cite-bibliography '())
  (setq org-cite-follow-processor 'citar)
  (setq org-cite-activate-processor 'citar)

  ;; Set Tectonic as the default compiler for Org LaTeX exports.
  (setq org-latex-compiler "tectonic")
  (setq org-latex-pdf-process
        '("tectonic -X compile %f -o %o"))

  ;; Define custom LaTeX classes for flexible document creation.
  (add-to-list 'org-latex-classes
        '("article"
           "\\documentclass{article}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")) t)
  (add-to-list 'org-latex-classes
        '("beamer"
           "\\documentclass{beamer}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")) t))
#+end_src

** Custom Snippets for Scientific Writing
#+begin_src emacs-lisp
(with-eval-after-load 'yasnippet
  ;; --- Snippet Definitions ---
  ;; We define the snippets programmatically to keep the config self-contained.
  (yas-define-snippets 'latex-mode
    '(;; -- Templates --
      ("article"
       "\\documentclass[11pt,a4paper]{article}
\\usepackage[utf8]{inputenc}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{graphicx}
\\usepackage{hyperref}
\\usepackage{siunitx}
\\usepackage{booktabs}

\\title{${1:Title}}
\\author{${2:Author}}
\\date{\\today}

\\begin{document}

\\maketitle

\\begin{abstract}
  ${3:Abstract}
\\end{abstract}

\\tableofcontents

\\section{${4:Introduction}}

$0

\\end{document}"
       "Full Scientific Article Structure"
       nil nil ("Templates"))

      ;; -- Document Structure & Environments --
      ("abs" "\\begin{abstract}\n  $0\n\\end{abstract}" "Abstract environment")
      ("fig"
       "\\begin{figure}[htbp]
  \\centering
  \\includegraphics[width=${1:0.8}\\textwidth]{${2:path/to/image}}
  \\caption{${3:Caption}}
  \\label{fig:${4:label}}
\\end{figure}
$0"
       "Figure environment")
      ("sfig"
       "\\begin{figure}[htbp]
  \\centering
  \\begin{subfigure}[b]{${1:0.45}\\textwidth}
    \\includegraphics[width=\\textwidth]{${2:img1}}
    \\caption{${3:Caption 1}}
    \\label{fig:${4:label1}}
  \\end{subfigure}
  \\hfill
  \\begin{subfigure}[b]{${1:0.45}\\textwidth}
    \\includegraphics[width=\\textwidth]{${5:img2}}
    \\caption{${6:Caption 2}}
    \\label{fig:${7:label2}}
  \\end{subfigure}
  \\caption{${8:Overall caption}}
\\end{figure}
$0"
       "Subfigure environment")
      ("table"
       "\\begin{table}[htbp]
  \\centering
  \\caption{${1:Caption}}
  \\label{tab:${2:label}}
  \\begin{tabular}{${3:l c r}}
    \\toprule
    ${4:Header 1} & ${5:Header 2} & ${6:Header 3} \\\\
    \\midrule
    ${7:data} & ${8:data} & ${9:data} \\\\
    \\bottomrule
  \\end{tabular}
\\end{table}
$0"
       "Table with booktabs")
      ("item" "\\begin{itemize}\n  \\item $0\n\\end{itemize}" "Itemize environment")
      ("enum" "\\begin{enumerate}\n  \\item $0\n\\end{enumerate}" "Enumerate environment")
      ("thm" "\\begin{theorem}\n  $0\n\\end{theorem}" "Theorem environment")
      ("lem" "\\begin{lemma}\n  $0\n\\end{lemma}" "Lemma environment")
      ("prf" "\\begin{proof}\n  $0\n\\end{proof}" "Proof environment")

      ;; -- Equations & Math --
      ("eq"
       "\\begin{equation}
  ${1:e^{i\\pi} + 1 = 0}
  \\label{eq:${2:label}}
\\end{equation}
$0"
       "Equation environment")
      ("ali"
       "\\begin{align}
  ${1:a} &= ${2:b} \\\\
  ${3:c} &= ${4:d}
  \\label{eq:${5:label}}
\\end{align}
$0"
       "Align environment for multi-line equations")
      ("mat"
       "\\begin{pmatrix}\n  ${1:a} & ${2:b} \\\\\n  ${3:c} & ${4:d}\n\\end{pmatrix}"
       "pmatrix (Matrix)")
      ("bmat"
       "\\begin{bmatrix}\n  ${1:a} & ${2:b} \\\\\n  ${3:c} & ${4:d}\n\\end{bmatrix}"
       "bmatrix (Bracketed Matrix)")
      ("lrp" "\\\\left( $1 \\\\right) $0" "Left-right parentheses")
      ("lrb" "\\\\left[ $1 \\\\right] $0" "Left-right brackets")
      ("lrc" "\\\\left\\{ $1 \\\\right\\} $0" "Left-right curly braces")
      ("sum" "\\\\sum_{${1:n=1}}^{${2:\\infty}} ${3:x_n}" "Summation")
      ("prod" "\\\\prod_{${1:n=1}}^{${2:\\infty}} ${3:x_n}" "Product")
      ("int" "\\\\int_{${1:a}}^{${2:b}} ${3:f(x)\\,dx}" "Integral")

      ;; -- Physics Specific --
      ("pd" "\\\\frac{\\\\partial ${1:y}}{\\\\partial ${2:x}} $0" "Partial derivative")
      ("dd" "\\\\frac{d ${1:y}}{d ${2:x}} $0" "Total derivative")
      ("bra" "\\\\bra{${1:\\psi}}$0" "Bra vector")
      ("ket" "\\\\ket{${1:\\psi}}$0" "Ket vector")
      ("braket" "\\\\braket{${1:\\psi}|${2:\\phi}}$0" "Braket inner product")
      ("h" "\\\\hbar" "hbar symbol")

      ;; -- Greek Letters (Prefix `g` for lowercase, `G` for uppercase) --
      ("ga" "\\\\alpha" "alpha")
      ("gb" "\\\\beta" "beta")
      ("gg" "\\\\gamma" "gamma")
      ("gd" "\\\\delta" "delta")
      ("ge" "\\\\epsilon" "epsilon")
      ("gz" "\\\\zeta" "zeta")
      ("go" "\\\\omega" "omega")
      ("GA" "\\\\Alpha" "Alpha")
      ("GB" "\\\\Beta" "Beta")
      ("GG" "\\\\Gamma" "Gamma")
      ("GD" "\\\\Delta" "Delta")
      ("GO" "\\\\Omega" "Omega")

      ;; -- Referencing & Citations --
      ("ref" "\\\\ref{${1:fig:label}}$0" "Reference")
      ("eqref" "\\\\eqref{${1:eq:label}}$0" "Equation Reference")
      ("citp" "\\\\citep{${1:key}}$0" "Parenthetical citation")
      )))
#+end_src

** Keybindings
#+begin_src emacs-lisp
(ar/global-leader
  "c" '(:ignore t :wk "compile/cite")
  "c c" '(TeX-command-master :wk "Compile Document")
  "c v" '(TeX-view :wk "View Output")
  "c e" '(TeX-clean :wk "Clean Aux Files")
  "c b" '(citar-insert-citation :wk "Insert Citation")
  "c o" '(citar-open :wk "Open Reference"))
#+end_src



```
