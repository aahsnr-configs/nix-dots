# Emacs + Home Manager Quick Start Guide

## 🚀 Get Started in 5 Minutes

This guide will help you set up a fully functional Emacs environment using the enhanced Home Manager module.

## Prerequisites

- NixOS or Nix package manager installed
- Home Manager configured
- Basic familiarity with Nix (optional but helpful)

## Step 1: Set Up Your Files

Create a directory for your Emacs configuration:

```bash
mkdir -p ~/.config/home-manager/emacs
cd ~/.config/home-manager/emacs
```

Copy the three main files:

- `emacs.nix` (the main module)
- `init.el` (example configuration provided)
- `early-init.el` (optional but recommended)

## Step 2: Integrate with Home Manager

Edit your `~/.config/home-manager/home.nix`:

```nix
{ config, pkgs, ... }:

{
  imports = [
    ./emacs/emacs.nix
  ];

  # ... rest of your config
}
```

## Step 3: Optional - Add emacs-overlay

For the latest Emacs versions, add emacs-overlay to your flake.nix:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { nixpkgs, home-manager, emacs-overlay, ... }: {
    homeConfigurations."yourusername" = home-manager.lib.homeManagerConfiguration {
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = [ emacs-overlay.overlays.default ];
      };
      modules = [ ./home.nix ];
    };
  };
}
```

## Step 4: Build and Activate

```bash
home-manager switch
```

This will:

- ✅ Install Emacs with native compilation
- ✅ Install all configured packages
- ✅ Link your init.el to ~/.config/emacs/
- ✅ Set up language servers and tools
- ✅ Configure environment variables

## Step 5: First Launch

```bash
emacs
```

### First-Time Setup

1. **Install Icon Fonts** (required for doom-modeline):

   ```
   M-x all-the-icons-install-fonts
   ```

   Press `y` when prompted, then restart Emacs.

2. **Verify Package Loading**:

   ```
   M-x list-packages
   ```

   You should see "No packages available" - this is correct! Packages are managed by Nix.

3. **Test LSP** (optional):
   - Open a `.nix` file
   - Check mode line for "Eglot" indicator
   - Try `M-x eglot-ensure` if not automatic

## Common Commands to Know

| Command | Action                        |
| ------- | ----------------------------- |
| `C-x g` | Open Magit (Git interface)    |
| `C-c p` | Projectile (project commands) |
| `C-x b` | Switch buffer (Consult)       |
| `M-s r` | Search with ripgrep           |
| `C-.`   | Embark actions menu           |
| `C-h f` | Describe function (Helpful)   |
| `M-o`   | Switch window (Ace-window)    |

## Customizing Your Setup

### Add a New Package

1. **Find the package name**:

   ```bash
   nix search nixpkgs emacsPackages.package-name
   ```

2. **Add to emacs.nix**:

   ```nix
   extraPackages = epkgs: with epkgs; [
     # ... existing packages
     new-package
   ];
   ```

3. **Configure in init.el**:

   ```elisp
   (use-package new-package
     :defer t
     :config
     (your-configuration-here))
   ```

4. **Rebuild**:
   ```bash
   home-manager switch
   ```

### Change Theme

Edit `init.el`:

```elisp
;; Replace the theme section with:
(use-package doom-themes
  :demand t
  :config
  (load-theme 'doom-tomorrow-night t))  ; Change this line
```

Popular themes included:

- `doom-one` (default dark)
- `doom-one-light` (light variant)
- `doom-dracula`
- `doom-gruvbox`
- `doom-nord`
- `doom-tomorrow-night`

Rebuild after changes:

```bash
home-manager switch
```

### Enable Emacs Daemon

For instant startup, edit `emacs.nix`:

```nix
services.emacs = {
  enable = true;  # Change to true
  socketActivation.enable = true;
};
```

Rebuild, then use:

```bash
emacsclient -c  # GUI frame
emacsclient -t  # Terminal frame
```

## Troubleshooting

### Problem: "Command not found: emacs"

**Solution**: Ensure Home Manager switch completed successfully:

```bash
home-manager switch --show-trace
```

Check if Emacs is in your PATH:

```bash
which emacs
ls -la ~/.nix-profile/bin/emacs
```

### Problem: Icons show as boxes

**Solution**: Install icon fonts from within Emacs:

```
M-x all-the-icons-install-fonts
```

Then restart Emacs completely.

### Problem: LSP not working

**Solution**:

1. Check if language server is installed:

   ```bash
   which nil  # for Nix
   which rust-analyzer  # for Rust
   ```

2. Manually start LSP:

   ```
   M-x eglot
   ```

3. Check Eglot events:
   ```
   M-x eglot-events-buffer
   ```

### Problem: Slow startup

**Solutions**:

1. Enable daemon mode (see above)
2. Check startup time:
   ```
   M-x emacs-init-time
   ```
3. Profile initialization:
   ```bash
   emacs --debug-init
   ```

### Problem: Package not found in init.el

**Error**: `Cannot open load file: No such file or directory, package-name`

**Solution**:

1. Verify package is in `extraPackages` in `emacs.nix`
2. Rebuild: `home-manager switch`
3. Remove `:ensure t` from use-package declaration
4. Restart Emacs completely

## Next Steps

### Learn More

1. **Org Mode**: Press `C-h i` then select Org Mode manual
2. **Magit**: Open a git repo and press `C-x g`
3. **Projectile**: Open a project directory and press `C-c p`
4. **Vertico/Consult**: Try `M-x consult-line` for fuzzy search

### Customize Further

- **Add more languages**: Edit the LSP hooks in init.el
- **Custom keybindings**: Use the `general` package and `leader-def`
- **Org capture templates**: Configure in the org-mode section
- **Personal functions**: Add to the Custom Functions section

### Join the Community

- \*\*Nix
