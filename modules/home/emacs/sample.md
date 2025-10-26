```org
* Completion Framework
** Orderless for Advanced Filtering
#+begin_src emacs-lisp
  (use-package orderless
    :ensure t
    :config
    ;; Configuration to be evaluated before the package is loaded.
    ;; Define a special, more restrictive completion style for Corfu.
    ;; This style only matches characters literally and in order, which prevents
    ;; the auto-completion popup from feeling chaotic.
    (orderless-define-completion-style orderless-literal-only
      (orderless-style-dispatchers nil)
      (orderless-matching-styles '(orderless-literal)))

    ;; Use a hook to apply our special style only when Corfu is active.
    ;; This is the key to having powerful filtering for commands (like M-x)
    ;; but simple, predictable filtering for auto-completion.
    (add-hook 'corfu-mode-hook
              (lambda ()
                (setq-local completion-styles '(orderless-literal-only basic)
                            completion-category-overrides nil
                            completion-category-defaults nil)))

    ;; Global settings managed by use-package.
    :custom
    ;; Use orderless as the primary completion style globally.
    (completion-styles '(orderless basic))
    (completion-category-defaults nil)

    ;; Use standard completion for file paths for a more predictable experience.
    (orderless-component-separator #'orderless-escapable-split-on-space)
    (completion-category-overrides '((file (styles basic partial-completion))))

    ;; Add dispatchers for more precise filtering (e.g., =literal, %regexp)
    ;; This enables advanced features for Consult/Vertico.
    (orderless-dispatchers
     '(orderless-consult-dispatch orderless-affix-dispatch)))
#+end_src

** Vertico: The Vertical Completion UI
#+begin_src emacs-lisp
(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :custom
  (vertico-resize nil)
  (vertico-cycle t)
  (vertico-count 10))
#+end_src

** Marginalia
#+begin_src emacs-lisp
(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))
#+end_src

** Nerd Icons Completion
#+begin_src emacs-lisp
(use-package nerd-icons-completion
  :ensure t
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  (nerd-icons-completion-mode))
#+end_src

** Consult
#+begin_src emacs-lisp
(use-package consult
  :ensure t
  :bind
  ;; Remap default commands to their Consult versions for a unified interface.
  ([remap switch-to-buffer] . consult-buffer)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
  ([remap bookmark-jump] . consult-bookmark)
  ([remap evil-show-marks] . consult-mark)
  ([remap evil-show-jumps] . consult-jump-list)
  ([remap goto-line] . consult-goto-line)
  ([remap imenu] . consult-imenu)
  ([remap load-theme] . consult-theme)
  ([remap recentf-open-files] . consult-recent-file)
  ([remap yank-pop] . consult-yank-pop)

  ;; Enable automatic preview at point in the *Completions* buffer. This is relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)


  :init
  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves ;; register formatting, adds thin separator lines, register sorting and hides the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (setq consult-prompt-margin 0)
  (setq consult-preview-key 'any)

  :custom
  ;; This is the new line that restricts consult-buffer to only show open buffers.
  (consult-buffer-sources '(consult--source-buffer))

  :config
  ;; Configure preview keys for various commands.
  ;; A delayed preview is used to avoid performance issues.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any)))
#+end_src

** Embark
#+begin_src emacs-lisp
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (define-key embark-collect-mode-map (kbd "e") #'embark-export)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
#+end_src

** Embark Consult
#+begin_src emacs-lisp
(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
#+end_src

** Corfu: The Core UI with Smart TAB Navigation
#+begin_src emacs-lisp
(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)

  ;; Configuration that must run *before* the package is loaded.
  :init
  ;; Keep a history of completion inputs.
  (corfu-history-mode)

  ;; Enable a popup with documentation for the selected candidate.
  (corfu-popupinfo-mode)

  ;; A list of modes where Corfu should be disabled to prevent conflicts.
  (defvar ar/corfu-disabled-modes
    '(erc-mode
      circe-mode
      help-mode
      gud-mode
      vterm-mode
      eshell-mode
      term-mode
      shell-mode
      comint-mode))

  ;; Configuration that runs *after* the package is loaded.
  :config
  ;; Turn off Corfu in the modes defined in our list.
  (add-hook 'corfu-mode-hook
            (lambda ()
              (when (memq major-mode ar/corfu-disabled-modes)
                (corfu-mode -1))))

  :custom
  (corfu-cycle t)                       ; Allow cycling from last to first candidate
  (corfu-auto t)                        ; Enable auto-completion
  (corfu-auto-resize nil)               ; Do not resize the popup
  (corfu-auto-delay 0.13)               ; Delay before auto-completion appears
  (corfu-preselect 'prompt)             ; Pre-select the first candidate
  (corfu-quit-at-boundary 'separator)   ; Quit completion when you cross a boundary (e.g., space)
  (corfu-quit-no-match 'separator)      ; Quit if there are no matches
  (corfu-on-exact-match nil)            ; Do not automatically select an exact match

  :bind
  (:map corfu-map
        ("TAB" . corfu-next)            ; Use TAB to navigate to next candidate
        ([tab] . corfu-next)            ; Also bind the tab key event
        ("S-TAB" . corfu-previous)      ; Use Shift-TAB to navigate to previous candidate
        ([backtab] . corfu-previous)))  ; Alternative binding for Shift-TAB
#+end_src

** Smart TAB Configuration
#+begin_src emacs-lisp
(use-package emacs
  :ensure nil
  :custom
  ;; TAB cycle if there are only few candidates (3 or less)
  (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; TAB will first try to indent the line. If the line is already indented,
  ;; it will then trigger completion. This is the recommended approach.
  (tab-always-indent 'complete)

  ;; Control when TAB completes on first press vs requiring a second press
  ;; nil: always complete immediately (if line is already indented)
  ;; 'eol: only complete if point is at end of line
  ;; 'word: complete unless next char has word syntax
  ;; 'word-or-paren: complete unless next char is word or parenthesis
  (tab-first-completion 'word-or-paren)

  ;; Hide commands in M-x which do not work in the current mode.
  ;; Corfu commands are hidden since they are not meant to be used via M-x.
  (read-extended-command-predicate #'command-completion-default-include-p))
#+end_src

** Nerd Icons for Corfu
#+begin_src emacs-lisp
(use-package nerd-icons-corfu
  :ensure t
  :after (corfu nerd-icons)
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
#+end_src

** Cape: Completion Backends
#+begin_src emacs-lisp
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  :config
  ;; Silence the noisy pcomplete capf
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent))
#+end_src

** Dabbrev
#+begin_src emacs-lisp
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Available since Emacs 29 (Use `dabbrev-ignored-buffer-regexps' on older Emacs)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))
#+end_src


```
