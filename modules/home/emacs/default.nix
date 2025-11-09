{pkgs, ...}: let
  # Create early-init.el content
  earlyInitContent = ''
    ;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-
    ;;; Commentary:
    ;; Emacs 27+ early initialization file, loaded before init.el,
    ;; before package and UI initialization happens.
    ;;; Code:

    ;; --- Garbage Collection Optimization ---
    ;; Defer garbage collection during startup for faster load times.
    (setq gc-cons-threshold most-positive-fixnum
          gc-cons-percentage 0.6)

    ;; --- File Name Handler Optimization ---
    ;; Temporarily disable file-name-handler-alist for faster startup.
    ;; This defers expensive features like TRAMP until after startup.
    (defvar ar/file-name-handler-alist-original file-name-handler-alist)
    (setq file-name-handler-alist nil)

    ;; --- Package System ---
    ;; Disable package.el - CRITICAL for Nix/Home-Manager setups.
    (setq package-enable-at-startup nil)

    ;; --- UI Element Optimization ---
    ;; Prevent flashes of unstyled content by disabling UI elements early.
    (push '(menu-bar-lines . 0) default-frame-alist)
    (push '(tool-bar-lines . 0) default-frame-alist)
    (push '(vertical-scroll-bars) default-frame-alist)

    ;; Disable mode-line initially
    (setq-default mode-line-format nil)

    ;; --- Frame Optimization ---
    (setq frame-inhibit-implied-resize t
          frame-resize-pixelwise t)

    ;; --- Startup Screen ---
    (setq inhibit-startup-screen t
          inhibit-startup-echo-area-message user-login-name
          inhibit-startup-buffer-menu t)

    ;; --- Initial Major Mode ---
    (setq initial-major-mode 'fundamental-mode)

    ;; --- Bidirectional Text Optimization ---
    (setq-default bidi-display-reordering 'left-to-right
                  bidi-paragraph-direction 'left-to-right)
    (setq bidi-inhibit-bpa t)

    ;; --- Performance Optimizations ---
    (setq idle-update-delay 1.0)
    (setq-default cursor-in-non-selected-windows nil)
    (setq highlight-nonselected-windows nil)
    (setq fast-but-imprecise-scrolling t)
    (setq inhibit-compacting-font-caches t)

    ;; --- Restore Settings After Startup ---
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Restore settings deferred during startup."
                (setq gc-cons-threshold (* 16 1024 1024)
                      gc-cons-percentage 0.1)
                (setq file-name-handler-alist ar/file-name-handler-alist-original)
                (setq-default mode-line-format (default-value 'mode-line-format))
                (message "Emacs startup complete. Restored deferred settings."))
              100)

    ;; --- Silence Compiler Warnings ---
    (setq native-comp-async-report-warnings-errors nil)
    (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
    ;;; early-init.el ends here
  '';
in {
  home.packages = [
    (pkgs.emacsWithPackagesFromUsePackage {
      config = ./config.org;
      defaultInitFile = true;
      package = pkgs.emacs-pgtk;
      alwaysEnsure = false;
      alwaysTangle = true;

      extraEmacsPackages = epkgs:
        with epkgs; [
          use-package
          vterm
          adaptive-wrap
          visual-fill-column
          auto-dim-other-buffers
          treesit-grammars.with-all-grammars
          minions
          telephone-line
        ];

      override = final: prev: {
        # Use built-in org-mode instead of the version from repositories
        org = null;
      };
    })
  ];

  # Create early-init.el file
  home.file.".emacs.d/early-init.el".text = earlyInitContent;
}
