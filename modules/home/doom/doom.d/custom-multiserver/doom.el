;;; packages.el --- lsp-bridge package configuration for Doom Emacs

;; Add lsp-bridge to packages.el
(when (package! lsp-bridge
        :recipe (:host github
                 :repo "manateelazycat/lsp-bridge"
                 :branch "master"
                 :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                 ;; Do not perform byte compilation or native compilation for lsp-bridge
                 :build (:not compile)))
  ;; Required dependencies
  (package! yasnippet)
  (package! markdown-mode))

;;; config.el --- lsp-bridge configuration for Doom Emacs

(use-package! lsp-bridge
  :config
  ;; Enable lsp-bridge globally
  (global-lsp-bridge-mode)
  
  ;; Set custom multiserver configuration directory
  ;; lsp-bridge will automatically read JSON files from this directory
  (setq lsp-bridge-user-multiserver-dir "~/.config/doom/lsp-bridge/multiserver")
  
  ;; Optional: Set custom single-server directory if needed
  (setq lsp-bridge-user-langserver-dir "~/.config/doom/lsp-bridge/langserver")
  
  ;; OPTION 1: Automatic detection by file extension
  ;; Add your multiserver configurations to this list
  ;; lsp-bridge will automatically use them based on file extension
  (setq lsp-bridge-multi-lang-server-extension-list
        '((("py") . "basedpyright_ruff")
          (("ts" "tsx") . "tsserver_eslint")
          (("js" "jsx") . "tsserver_eslint")
          (("vue") . "volar_eslint")
          (("rs") . "rust_analyzer_clippy")
          (("go") . "gopls_golangci")
          (("c" "cpp" "cc" "h" "hpp") . "clangd_clang_tidy")
          (("html") . "html_css_json")))
  
  ;; OPTION 2: Automatic detection by major-mode
  ;; Alternative way to specify multiserver by Emacs mode
  ;; (add-to-list 'lsp-bridge-multi-lang-server-mode-list
  ;;              '(python-mode . "basedpyright_ruff"))
  ;; (add-to-list 'lsp-bridge-multi-lang-server-mode-list
  ;;              '(typescript-mode . "tsserver_eslint"))
  
  ;; OPTION 3: Custom project-based selection (most flexible)
  ;; Only use this if you need dynamic selection based on project or file content
  ;; This overrides the extension-list and mode-list options above
  ;; (setq lsp-bridge-get-multi-lang-server-by-project
  ;;       (lambda (project-path filepath)
  ;;         (cond
  ;;          ;; Python projects - use basedpyright + ruff
  ;;          ((string-suffix-p ".py" filepath)
  ;;           "basedpyright_ruff")
  ;;          
  ;;          ;; JavaScript/TypeScript projects - use tsserver + eslint
  ;;          ((or (string-suffix-p ".ts" filepath)
  ;;               (string-suffix-p ".tsx" filepath)
  ;;               (string-suffix-p ".js" filepath)
  ;;               (string-suffix-p ".jsx" filepath))
  ;;           "tsserver_eslint")
  ;;          
  ;;          ;; Vue.js projects
  ;;          ((string-suffix-p ".vue" filepath)
  ;;           "volar_eslint")
  ;;          
  ;;          ;; Default: return nil to use extension/mode-based detection
  ;;          (t nil))))
  
  ;; Custom save buffer function for remote files
  (defun my/save-buffer ()
    "Save buffer with lsp-bridge remote file support."
    (interactive)
    (if lsp-bridge-remote-file-flag
        (call-interactively #'lsp-bridge-remote-save-buffer)
      (call-interactively #'save-buffer)))
  
  ;; Key bindings
  (map! :leader
        :desc "LSP Bridge restart process" "c R" #'lsp-bridge-restart-process
        :desc "LSP Bridge code action" "c a" #'lsp-bridge-code-action
        :desc "LSP Bridge find definition" "c d" #'lsp-bridge-find-def
        :desc "LSP Bridge find references" "c r" #'lsp-bridge-find-references
        :desc "LSP Bridge rename" "c n" #'lsp-bridge-rename
        :desc "LSP Bridge diagnostic list" "c x" #'lsp-bridge-diagnostic-list
        :desc "LSP Bridge show signature" "c k" #'lsp-bridge-signature-help-fetch
        :desc "LSP Bridge workspace symbol" "c s" #'lsp-bridge-workspace-list-symbols)
  
  (map! "C-x C-s" #'my/save-buffer)
  
  ;; Optional: Enable remote file support via tramp
  ;; (setq lsp-bridge-enable-with-tramp t)
  )

;; Optional: Integrate with flymake-bridge for error checking
;; (when (package! flymake-bridge
;;         :recipe (:host github
;;                  :repo "liuyinz/flymake-bridge"
;;                  :branch "master"))
;;   (use-package! flymake-bridge
;;     :after lsp-bridge
;;     :hook (lsp-bridge-mode . flymake-bridge-setup)
;;     :config
;;     (map! :after flymake
;;           "M-n" #'flymake-goto-next-error
;;           "M-p" #'flymake-goto-prev-error)))

;; Optional: Format on save with apheleia
;; Enable (:editor format) in init.el first
;; (use-package! apheleia
;;   :after lsp-bridge
;;   :config
;;   ;; Don't mess up with lsp-mode
;;   (setq +format-with-lsp nil)
;;   (setq apheleia-remote-algorithm 'remote))
