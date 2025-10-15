# Declarative Emacs Home Manager Module - Enhanced Edition

## Overview

This Home Manager module provides a fully declarative, production-ready Emacs environment that integrates seamlessly with your existing `init.el` configuration. It follows current NixOS community best practices (2024/2025) and includes comprehensive error handling, fallbacks, and enhancements.

## 🔧 Fixes & Improvements from Original

### Critical Fixes

1. **✅ Package Name Resolution**: Fixed `emacs-pgtk` availability check with proper fallbacks
   - Now detects if emacs-overlay is installed
   - Falls back gracefully to `emacs29-pgtk` or `emacs-gtk` if overlay isn't available
2. **✅ Custom Package Building**: Fixed `trivialBuild` usage
   - Correctly uses `pkgs.emacsPackages.trivialBuild`
   - Added second example (`nano-theme`) for reference

3. **✅ Path Existence Checking**: Added `lib.mkIf (builtins.pathExists ...)` guards
   - Module won't fail if `init.el` or `early-init.el` is missing
   - Provides helpful warnings instead of build failures

4. **✅ `extraConfig` Clarification**: Properly documented and used
   - This content is prepended to init.el, not ignored
   - Now includes essential bootstrap configuration

5. **✅ Services Configuration**: Fixed all `services.emacs` options
   - Corrected option names (`socketActivation.enable`, etc.)
   - Added proper `finalPackage` reference
   - Documented all available options

### Major Enhancements

1. **🎯 Extended Package List**:
   - Added 30+ additional essential packages
   - Included completion alternatives (corfu/company)
   - Added navigation and editing enhancements
   - Included utility packages for better workflow

2. **🔧 Better LSP/Tool Support**:
   - Expanded language server list
   - Added formatters and linters
   - Included git tools for forge integration

3. **📝 Custom.el Management**:
   - Created separate `custom.el` file
   - Prevents Emacs from modifying `init.el`
   - Properly configured with `force = false`

4. **🖥️ Desktop Integration**:
   - Added XDG desktop entry for GUI integration
   - Configured MIME types for file associations
   - Proper integration with application menus

5. **🌳 Tree-sitter Environment**:
   - Added `TREE_SITTER_DIR` environment variable
   - Ensures Emacs can find Nix-installed grammars

6. **💪 Production-Ready Defaults**:
   - Comprehensive error handling
   - Graceful fallbacks for missing components
   - Better documentation and comments

## Prerequisites

### Required

1. **Home Manager**: This module requires Home Manager to be set up
2. **Your init.el file**: Must be in the same directory as this module

### Optional but Recommended

**emacs-overlay** for access to bleeding-edge Emacs builds:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, emacs-overlay, ... }: {
    nixosConfigurations.your-hostname = nixpkgs.lib.nixosSystem {
      modules = [
        {
          nixpkgs.overlays = [ emacs-overlay.overlays.default ];
        }
        home-manager.nixosModules.home-manager
        # ... other modules
      ];
    };
  };
}
```

## Directory Structure

```
.
├── emacs.nix          # This module file
├── init.el            # Your Emacs configuration (will be created if missing)
├── early-init.el      # Optional early initialization
└── README.md          # This documentation
```

## Installation & Usage

### Basic Integration

Add to your Home Manager configuration:

```nix
{ config, pkgs, ... }:

{
  imports = [
    ./emacs.nix
  ];
}
```

### First Time Setup

1. **Create your init.el** (if you don't have one):

```bash
cat > init.el << 'EOF'
;;; init.el --- My Emacs Configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Minimal starter configuration
;;; Code:

;; Set custom-file to prevent pollution of init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Use-package is already installed via Nix
(require 'use-package)
(setq use-package-always-defer t)  ; Lazy load by default

;; Example configuration
(use-package emacs
  :demand t
  :custom
  (user-full-name "Your Name")
  (user-mail-address "your.email@example.com")
  :config
  (set-language-environment "UTF-8"))

(provide 'init)
;;; init.el ends here
```

### Optional early-init.el

Create this file for pre-initialization optimizations:

```elisp
;;; early-init.el --- Early Init File -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded before init.el and package initialization

;;; Code:

;; Defer garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Prevent package.el from loading packages before init.el
(setq package-enable-at-startup nil)

;; Prevent Emacs from resizing the frame
(setq frame-inhibit-implied-resize t)

;; Disable unnecessary UI elements early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Native compilation settings
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t))

;; Increase the amount of data read from processes
(setq read-process-output-max (* 1024 1024)) ; 1MB

;;; early-init.el ends here
```

## Package Management

### Adding Packages

To add a new package from MELPA/ELPA/etc:

1. **Add to Nix module**:

```nix
extraPackages = epkgs: with epkgs; [
  # ... existing packages
  my-new-package
];
```

2. **Configure in init.el** (without `:ensure`):

```elisp
(use-package my-new-package
  :config
  ;; your configuration here
  )
```

3. **Rebuild**:

```bash
home-manager switch
```

### Finding Package Names

Search nixpkgs for Emacs packages:

```bash
nix search nixpkgs emacsPackages.magit
```

Or browse: https://search.nixos.org/packages?channel=unstable&query=emacsPackages

### Custom Packages from Git

Example of packaging an external package:

```nix
my-package = pkgs.emacsPackages.trivialBuild rec {
  pname = "my-package";
  version = "1.0.0";

  src = pkgs.fetchFromGitHub {
    owner = "username";
    repo = "my-package";
    rev = "commit-hash";
    sha256 = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  };

  # Declare elisp dependencies
  propagatedUserEnvPkgs = with pkgs.emacsPackages; [
    dependency1
    dependency2
  ];

  buildInputs = propagatedUserEnvPkgs;

  meta = with lib; {
    description = "My package description";
    license = licenses.gpl3Plus;
    platforms = platforms.all;
  };
};
```

Then add `my-package` to the `extraPackages` list.

To get the correct sha256:

```bash
nix-prefetch-git https://github.com/username/my-package commit-hash
```

## Tree-sitter Configuration

### Available Grammars

The module installs all tree-sitter grammars via `treesit-grammars.with-all-grammars`. This includes:

- bash, c, cpp, c-sharp
- css, dockerfile, go, html
- java, javascript, json, lua
- markdown, python, ruby, rust
- toml, tsx, typescript, yaml
- And many more...

### Using Tree-sitter Modes

Emacs 29+ includes built-in tree-sitter modes with `-ts-mode` suffix:

```elisp
;; Automatic remapping in init.el
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        (javascript-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (yaml-mode . yaml-ts-mode)))

;; Or use file associations
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
```

### Verifying Grammar Installation

Check if grammars are available:

```elisp
M-x treesit-install-language-grammar
```

List installed grammars:

```elisp
M-: (treesit-available-p) RET
M-: treesit-language-source-alist RET
```

## Language Server Setup (Eglot)

### Supported Languages

The module includes language servers for:

- **Nix**: `nil` (recommended) or `nixd`
- **TypeScript/JavaScript**: `typescript-language-server`
- **Python**: Available but commented (add `pyright` or `python-lsp-server`)
- **Rust**: `rust-analyzer`
- **Lua**: `lua-language-server`
- **Bash**: `bash-language-server`
- **HTML/CSS/JSON**: `vscode-langservers-extracted`

### Adding More Language Servers

1. **Add to home.packages**:

```nix
home.packages = with pkgs; [
  # ... existing packages
  gopls                  # Go
  clang-tools            # C/C++
  haskell-language-server  # Haskell
];
```

2. **Configure in init.el**:

```elisp
(use-package eglot
  :hook ((go-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)))
```

### Eglot Configuration Tips

```elisp
;; Customize eglot behavior
(use-package eglot
  :custom
  ;; Disable event logging for performance
  (eglot-events-buffer-size 0)

  ;; Shutdown server when last managed buffer is killed
  (eglot-autoshutdown t)

  ;; Don't auto-reconnect
  (eglot-autoreconnect nil)

  ;; Configure server programs
  :config
  (add-to-list 'eglot-server-programs
               '(nix-mode . ("nil")))
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) . ("rust-analyzer"))))
```

## Troubleshooting

### Issue: Package Not Found

**Problem**: Emacs can't find a package listed in `extraPackages`

**Solutions**:

1. Verify package exists in nixpkgs:

   ```bash
   nix search nixpkgs emacsPackages.package-name
   ```

2. Check the package builds:

   ```bash
   nix build .#homeConfigurations.your-user.config.programs.emacs.finalPackage
   ```

3. Ensure you're not using `:ensure t` in init.el

### Issue: Init.el Not Found Error

**Problem**: Build fails with "init.el not found"

**Solution**: The module now includes `lib.mkIf (builtins.pathExists ./init.el)` guards. If you still see errors:

1. Create a minimal init.el in the same directory:

   ```bash
   echo ";;; init.el ends here" > init.el
   ```

2. Or disable the file linking:
   ```nix
   # Comment out in emacs.nix:
   # home.file.".config/emacs/init.el" = lib.mkIf ...
   ```

### Issue: Tree-sitter Grammars Missing

**Problem**: Tree-sitter modes report missing grammars

**Solutions**:

1. Verify grammars are installed:

   ```bash
   ls ~/.nix-profile/lib/ | grep tree-sitter
   ```

2. Check `TREE_SITTER_DIR` environment variable:

   ```bash
   echo $TREE_SITTER_DIR
   ```

3. Restart Emacs completely (not just reload config)

### Issue: Icons Not Displaying

**Problem**: Doom modeline or all-the-icons show boxes instead of icons

**Solution**:

1. Install fonts from within Emacs (first time only):

   ```
   M-x all-the-icons-install-fonts
   ```

2. If using nerd-icons, restart Emacs after fonts are installed

3. On some systems, you may need to run:
   ```bash
   fc-cache -f -v
   ```

### Issue: Slow Startup

**Problem**: Emacs takes too long to start

**Solutions**:

1. Use the daemon:

   ```nix
   services.emacs.enable = true;
   ```

2. Profile startup time:

   ```
   emacs --debug-init
   ```

   or use:

   ```elisp
   M-x emacs-init-time
   ```

3. Ensure packages use `:defer t` or specific hooks:

   ```elisp
   (use-package my-package
     :defer t  ; or :hook, :bind, :mode
     ...)
   ```

4. Increase gc threshold during startup (in early-init.el)

### Issue: LSP Server Not Starting

**Problem**: Eglot doesn't connect to language server

**Solutions**:

1. Verify language server is installed:

   ```bash
   which nil  # or rust-analyzer, etc.
   ```

2. Check Eglot events buffer:

   ```
   M-x eglot-events-buffer
   ```

3. Manually start server:

   ```
   M-x eglot
   ```

4. Check server configuration:
   ```elisp
   M-: eglot-server-programs RET
   ```

### Issue: Custom.el Conflicts

**Problem**: Emacs writes to init.el despite custom-file setting

**Solution**: Ensure custom-file is loaded early:

```elisp
;; Must be before any other customization
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
```

## Advanced Configuration

### Using with Nix Flakes

Example flake.nix:

```nix
{
  description = "My system configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, emacs-overlay }: {
    homeConfigurations."user@hostname" = home-manager.lib.homeManagerConfiguration {
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = [ emacs-overlay.overlays.default ];
      };

      modules = [
        ./home.nix
        ./emacs.nix
      ];
    };
  };
}
```

### Per-Project Emacs Configuration

Use direnv + envrc for project-specific settings:

1. **Enable in module**:

   ```nix
   extraPackages = epkgs: with epkgs; [
     envrc  # or direnv
   ];
   ```

2. **Configure in init.el**:

   ```elisp
   (use-package envrc
     :demand t
     :config
     (envrc-global-mode 1))
   ```

3. **Create .envrc in project**:
   ```bash
   use flake
   ```

### Custom Emacs Builds

Override the Emacs package with patches:

```nix
programs.emacs.package = pkgs.emacs-pgtk.overrideAttrs (old: {
  patches = (old.patches or []) ++ [
    (pkgs.fetchpatch {
      url = "https://example.com/my-patch.patch";
      sha256 = "sha256-...";
    })
  ];
});
```

### Multiple Emacs Configs

Run different Emacs configurations:

```nix
home.file.".config/emacs-minimal/init.el".source = ./init-minimal.el;
```

Then start with:

```bash
emacs --init-directory ~/.config/emacs-minimal
```

## Performance Optimization

### Startup Optimization Checklist

- [ ] Use `early-init.el` for pre-initialization
- [ ] Set `gc-cons-threshold` high during startup
- [ ] Use `:defer t` on all non-essential packages
- [ ] Use hooks instead of `:demand t` where possible
- [ ] Enable native compilation
- [ ] Use tree-sitter modes instead of regex-based ones
- [ ] Profile with `M-x emacs-init-time`

### Runtime Optimization

```elisp
;; Increase process read limit
(setq read-process-output-max (* 1024 1024)) ; 1MB

;; LSP performance
(setq eglot-events-buffer-size 0)  ; Disable logging

;; Reduce rendering
(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)
```

## Comparison with Other Approaches

| Approach                            | Pros                                          | Cons                                 |
| ----------------------------------- | --------------------------------------------- | ------------------------------------ |
| **This Module**                     | Declarative, reproducible, version-controlled | Requires Nix knowledge               |
| **straight.el**                     | Git-based packages, easy updates              | Not declarative, state in ~/.emacs.d |
| **package.el**                      | Built-in, simple                              | Not reproducible, version conflicts  |
| **emacsWithPackagesFromUsePackage** | Parses use-package config                     | Limited flexibility, Unicode issues  |

## Migration Guide

### From package.el

1. List all installed packages:

   ```elisp
   M-: (mapcar 'car package-alist) RET
   ```

2. Add to `extraPackages` in emacs.nix

3. Remove `:ensure t` from all use-package declarations

4. Remove package.el initialization from init.el

### From straight.el

1. List packages from `straight/versions/default.el`

2. Find equivalent packages in nixpkgs

3. Add to `extraPackages`

4. Remove straight.el bootstrap code

5. Keep use-package configurations (remove :straight)

## Additional Resources

- **NixOS Wiki**: https://nixos.wiki/wiki/Emacs
- **emacs-overlay**: https://github.com/nix-community/emacs-overlay
- **Home Manager Manual**: https://nix-community.github.io/home-manager/
- **Emacs Manual**: https://www.gnu.org/software/emacs/manual/
- **Nix Pills**: https://nixos.org/guides/nix-pills/
- **use-package**: https://github.com/jwiegley/use-package

## Contributing

Found an issue or have an improvement? This module is designed to be a living template. Feel free to:

- Add more example packages
- Improve error handling
- Add more troubleshooting tips
- Share your custom package recipes

## License

This module is provided as a template for personal use. Adapt it freely to your needs.
EOF

````

2. **Build and activate**:

```bash
home-manager switch
````

3. **Install icon fonts** (first time only):

Open Emacs and run:

```
M-x all-the-icons-install-fonts
```

### Enable Emacs Daemon

To enable the Emacs daemon for instant startup, edit the module:

```nix
  services.emacs = {
    enable = true;  # Change from false to true
    socketActivation.enable = true;
  };
```

Then rebuild:

```bash
home-manager switch
```

Use the daemon with:

- `emacsclient -c` - Open GUI frame
- `emacsclient -t` - Open terminal frame
- `emacsclient -n file.txt` - Open file without waiting

## Package Selection Guide

### Emacs Variants Available

The module intelligently selects the best available Emacs:

| Package               | Source        | Description                  | Recommendation               |
| --------------------- | ------------- | ---------------------------- | ---------------------------- |
| `pkgs.emacs-pgtk`     | emacs-overlay | Latest with PGTK+native-comp | ⭐ Best for Wayland          |
| `pkgs.emacs-git`      | emacs-overlay | Bleeding edge from master    | ⚠️ Unstable                  |
| `pkgs.emacs-unstable` | emacs-overlay | Latest release tag           | ✅ Good balance              |
| `pkgs.emacs29-pgtk`   | nixpkgs       | Stable Emacs 29 with PGTK    | ✅ Good for stability        |
| `pkgs.emacs-gtk`      | nixpkgs       | Standard GTK build           | ⚠️ May be unstable as daemon |
| `pkgs.emacs`          | nixpkgs       | Lucid toolkit                | ❌ Ugly but stable           |

To override the automatic selection:

```nix
  programs.emacs.package = pkgs.emacs-unstable;  # Force specific version
```

## Writing Your init.el

### Critical Rules

1. **❌ Never use `:ensure t`** - Packages are managed by Nix
2. **✅ Always use `:defer t`** or loading triggers for performance
3. **✅ Set `custom-file`** to prevent init.el pollution

### Example init.el Structure

```elisp
;;; init.el --- My Emacs Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs configuration managed with Nix + Home Manager

;;; Code:

;; ========================================
;; Core Settings
;; ========================================

;; Redirect customize output
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Personal Information
(setq user-full-name "Your Name"
      user-mail-address "your.email@example.com")

;; Performance Tuning
(setq gc-cons-threshold (* 50 1000 1000))  ; 50MB
(setq read-process-output-max (* 1024 1024))  ; 1MB

;; ========================================
;; Use-package Configuration
;; ========================================

(require 'use-package)
(setq use-package-always-defer t
      use-package-expand-minimally t)

;; ========================================
;; UI Configuration
;; ========================================

(use-package emacs
  :demand t
  :custom
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (ring-bell-function 'ignore)
  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-fringe-mode 10)
  (global-display-line-numbers-mode t)
  (column-number-mode t)
  (size-indication-mode t))

;; Theme
(use-package doom-themes
  :demand t
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

;; Modeline
(use-package doom-modeline
  :demand t
  :custom
  (doom-modeline-height 25)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  :config
  (doom-modeline-mode 1))

;; Icons (required by doom-modeline)
(use-package all-the-icons
  :demand t)

;; Which-key for discoverability
(use-package which-key
  :demand t
  :custom
  (which-key-idle-delay 0.3)
  :config
  (which-key-mode 1))

;; ========================================
;; Completion Framework
;; ========================================

(use-package vertico
  :demand t
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode 1))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :demand t
  :config
  (marginalia-mode 1))

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-g g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("M-s d" . consult-find)
         ("M-s g" . consult-grep)))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  :after (embark consult))

;; Completion at point (choose one: corfu or company)
(use-package corfu
  :demand t
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-quit-no-match 'separator)
  :config
  (global-corfu-mode 1))

(use-package cape
  :after corfu
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; ========================================
;; Programming Support
;; ========================================

;; Eglot LSP (built-in to Emacs 29+)
(use-package eglot
  :hook ((nix-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (typescript-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0))

;; Syntax checking
(use-package flycheck
  :demand t
  :config
  (global-flycheck-mode 1))

;; Snippets
(use-package yasnippet
  :demand t
  :config
  (yas-global-mode 1))

;; Tree-sitter
(use-package treesit
  :when (treesit-available-p)
  :custom
  (treesit-font-lock-level 4)
  :config
  ;; Remap major modes to tree-sitter versions
  (setq major-mode-remap-alist
        '((python-mode . python-ts-mode)
          (javascript-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (rust-mode . rust-ts-mode))))

;; ========================================
;; Version Control
;; ========================================

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package diff-hl
  :demand t
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1))

;; ========================================
;; Project Management
;; ========================================

(use-package projectile
  :demand t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'default)
  (projectile-enable-caching t)
  :config
  (projectile-mode 1))

;; ========================================
;; Org Mode
;; ========================================

(use-package org
  :custom
  (org-directory "~/org")
  (org-agenda-files '("~/org"))
  (org-log-done 'time)
  (org-startup-indented t)
  :config
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture))

(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode))

;; ========================================
;; Navigation & Editing
;; ========================================

(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; ========================================
;; File Management
;; ========================================

(use-package dired
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (dired-dwim-target t))

;; ========================================
;; Restore garbage collection
;; ========================================

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold (* 2 1000 1000))))

(provide 'init)
;;; init.el ends here
```
