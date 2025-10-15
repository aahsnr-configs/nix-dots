;;; early-init.el --- Early Init File -*- lexical-binding: t; -*-

;;; Commentary:
;; This file is loaded before init.el and before package initialization.
;; Use it for:
;; - UI optimizations that should happen before the first frame
;; - Native compilation settings
;; - Performance tuning that must happen early
;; - Disabling package.el (when using Nix)

;;; Code:

;; ============================================================================
;; Garbage Collection Optimization
;; ============================================================================

;; Defer garbage collection during startup for faster loading
;; This will be restored to a reasonable value at the end of init.el
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; ============================================================================
;; Package Management
;; ============================================================================

;; Disable package.el completely since we're using Nix
;; This prevents Emacs from loading packages from ~/.emacs.d/elpa
(setq package-enable-at-startup nil)

;; ============================================================================
;; Native Compilation Settings
;; ============================================================================

(when (featurep 'native-compile)
  ;; Silence compiler warnings (they're often not actionable)
  (setq native-comp-async-report-warnings-errors nil)
  
  ;; Compile packages asynchronously
  (setq native-comp-deferred-compilation t)
  
  ;; Set the native-comp cache directory
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (expand-file-name "eln-cache/" user-emacs-directory))))

;; ============================================================================
;; UI Optimization
;; ============================================================================

;; Prevent frame from resizing during startup
;; This reduces flickering
(setq frame-inhibit-implied-resize t)

;; Disable GUI elements early to prevent them from appearing
;; This provides a cleaner startup experience
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Disable the startup screen early
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message user-login-name)

;; Don't create a new frame for files
(setq pop-up-frames nil)

;; ============================================================================
;; Performance Tuning
;; ============================================================================

;; Increase the amount of data Emacs can read from a process
;; This is especially important for LSP servers
(setq read-process-output-max (* 1024 1024)) ; 1MB

;; Reduce rendering workload by not rendering cursors or regions in non-focused windows
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names
(setq ffap-machine-p-known 'reject)

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)

;; ============================================================================
;; File Handling
;; ============================================================================

;; Disable bidirectional text rendering for a modest performance boost
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Reduce cursor lag by a tiny bit by not rendering cursors in non-focused windows
(setq-default cursor-in-non-selected-windows nil)

;; Don't compact font caches during GC (improves performance with icon fonts)
(setq inhibit-compacting-font-caches t)

;; ============================================================================
;; macOS Specific Settings
;; ============================================================================

(when (eq system-type 'darwin)
  ;; Use proper title bar on macOS
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  
  ;; Disable the macOS-specific menu bar
  (setq frame-title-format nil))

;; ============================================================================
;; Load Path Optimization
;; ============================================================================

;; Resetting the load-path can speed up startup
;; Only do this if you're absolutely sure about your configuration
;; (setq load-path (list (expand-file-name "lisp" user-emacs-directory)))

;;; early-init.el ends here
