
;; --- Manually Define XDG-Compliant Paths ---
(defvar no-littering-etc-directory
  (expand-file-name "etc/" user-emacs-directory)
  "Directory for configuration files.")
(defvar no-littering-var-directory
  (expand-file-name "var/" user-emacs-directory)
  "Directory for mutable data files.")
(defvar no-littering-cache-directory
  (expand-file-name "cache/" user-emacs-directory)
  "Directory for cache files.")

;; --- Ensure Directories Exist ---
(dolist (dir (list no-littering-etc-directory
                   no-littering-var-directory
                   no-littering-cache-directory
                   (expand-file-name "backups/" no-littering-var-directory)
                   (expand-file-name "auto-saves/" no-littering-var-directory)
                   (expand-file-name "server/" no-littering-var-directory)))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; --- Relocate Core Emacs Files ---
(setq custom-file (expand-file-name "custom.el" no-littering-etc-directory))
(setq package-user-dir (expand-file-name "elpa/" no-littering-var-directory))
(setq url-history-file (expand-file-name "url-history" no-littering-var-directory))
(setq recentf-save-file (expand-file-name "recentf" no-littering-var-directory))
(setq savehist-file (expand-file-name "savehist" no-littering-var-directory))
(setq save-place-file (expand-file-name "saveplace" no-littering-var-directory))
(setq bookmark-save-file (expand-file-name "bookmarks" no-littering-var-directory))
(setq abbrev-file-name (expand-file-name "abbrevs.el" no-littering-var-directory))
(setq tramp-persistency-file-name (expand-file-name "tramp" no-littering-var-directory))
(setq server-dir (expand-file-name "server/" no-littering-var-directory))

;; --- Relocate Backup and Auto-Save Files ---
(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" no-littering-var-directory)))
      auto-save-list-file-prefix (expand-file-name "auto-save-list/" no-littering-var-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-saves/" no-littering-var-directory) t)))

;; --- Relocate Native Compilation Cache (for Emacs 29+) ---
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (expand-file-name "eln-cache" no-littering-cache-directory)))
