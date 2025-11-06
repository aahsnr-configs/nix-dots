{
  inputs,
  pkgs,
  config,
  lib,
  ...
}: let
  # Path to home-manager flake configuration
  flakeDir = "${config.home.homeDirectory}/.config/home-manager";

  # Script to automatically rebuild home-manager when colors change
  autoRebuildScript = pkgs.writeShellScript "auto-rebuild-niri-theme.sh" ''
    #!/usr/bin/env bash
    set -euo pipefail

    LOG_PREFIX="[auto-rebuild-niri-theme]"
    DMS_COLORS="${config.home.homeDirectory}/.config/DankMaterialShell/dms-colors.json"
    FLAKE_DIR="${flakeDir}"
    BACKUP_DIR="${config.home.homeDirectory}/.config/niri/backups"

    log() {
      echo "$LOG_PREFIX $1" >&2
    }

    error() {
      log "ERROR: $1"
      exit 1
    }

    # Check if DMS colors file exists
    if [ ! -f "$DMS_COLORS" ]; then
      log "DMS colors file not found, skipping rebuild"
      exit 0
    fi

    # Validate JSON
    if ! ${pkgs.jq}/bin/jq empty "$DMS_COLORS" 2>/dev/null; then
      error "Invalid JSON in DMS colors file"
    fi

    log "DMS colors changed, triggering home-manager rebuild..."

    # Create backup directory
    mkdir -p "$BACKUP_DIR"

    # Backup current niri config if it exists
    NIRI_CONFIG="${config.home.homeDirectory}/.config/niri/config.kdl"
    if [ -f "$NIRI_CONFIG" ]; then
      BACKUP_FILE="$BACKUP_DIR/config.kdl.backup-$(date +%Y%m%d-%H%M%S)"
      cp "$NIRI_CONFIG" "$BACKUP_FILE"
      log "Backed up niri config to $BACKUP_FILE"
    fi

    # Change to flake directory
    cd "$FLAKE_DIR" || error "Flake directory not found: $FLAKE_DIR"

    # Ensure files are tracked by git (flakes require this)
    if [ ! -d ".git" ]; then
      log "WARNING: Not a git repository. Consider running: git init && git add ."
    else
      # Add any modified/new files to git index (but don't commit)
      # This is required for flakes which only include files tracked by git
      ${pkgs.git}/bin/git add -A 2>/dev/null || true
    fi

    # Build and switch home-manager configuration
    log "Running home-manager switch..."

    # Use home-manager from PATH (it should already be installed)
    if command -v home-manager >/dev/null 2>&1; then
      if home-manager switch --flake "$FLAKE_DIR"; then
        log "✓ Home-manager rebuild successful"
        log "✓ Niri will automatically reload with new colors"

        # Extract and log the new colors
        PRIMARY=$(${pkgs.jq}/bin/jq -r '.primary // "#b1c5ff"' "$DMS_COLORS")
        INACTIVE=$(${pkgs.jq}/bin/jq -r '.surfaceVariantText // "#8f9099"' "$DMS_COLORS")
        URGENT=$(${pkgs.jq}/bin/jq -r '.error // "#ffb4ab"' "$DMS_COLORS")

        log "New colors applied:"
        log "  Primary: $PRIMARY"
        log "  Inactive: $INACTIVE"
        log "  Urgent: $URGENT"
      else
        error "Home-manager rebuild failed! Check logs above."
      fi
    else
      error "home-manager command not found in PATH"
    fi
  '';
in {
  imports = [
    inputs.dankMaterialShell.homeModules.dankMaterialShell.default
    inputs.dankMaterialShell.homeModules.dankMaterialShell.niri
  ];

  programs.dankMaterialShell = {
    enable = true;
    niri.enableSpawn = true;
    quickshell.package = inputs.quickshell.packages.${pkgs.system}.default;
    default.settings = {
      theme = "dark";
      dynamicTheming = true;
    };
  };

  # Install dependencies and helper scripts
  home.packages = with pkgs; [
    jq
    matugen
    # Helper scripts
    (writeShellScriptBin "dms-trigger-rebuild" ''
      #!/usr/bin/env bash
      echo "Manually triggering home-manager rebuild..."
      systemctl --user start auto-rebuild-niri-theme.service
    '')

    (writeShellScriptBin "dms-show-colors" ''
      #!/usr/bin/env bash
      DMS_COLORS="${config.home.homeDirectory}/.config/DankMaterialShell/dms-colors.json"

      if [ ! -f "$DMS_COLORS" ]; then
        echo "ERROR: DMS colors file not found"
        echo "Make sure DankMaterialShell is running and has generated colors"
        exit 1
      fi

      echo "Current DMS Colors:"
      echo "=================="
      ${pkgs.jq}/bin/jq -C '.' "$DMS_COLORS"
    '')

    (writeShellScriptBin "dms-disable-auto-rebuild" ''
      #!/usr/bin/env bash
      echo "Disabling automatic rebuilds..."
      systemctl --user stop watch-dms-colors.path
      systemctl --user disable watch-dms-colors.path
      echo "✓ Auto-rebuild disabled"
      echo "To re-enable: systemctl --user enable --now watch-dms-colors.path"
    '')

    (writeShellScriptBin "dms-enable-auto-rebuild" ''
      #!/usr/bin/env bash
      echo "Enabling automatic rebuilds..."
      systemctl --user enable --now watch-dms-colors.path
      echo "✓ Auto-rebuild enabled"
      echo "Wallpaper changes will now automatically update Niri colors"
    '')

    (writeShellScriptBin "dms-rebuild-status" ''
      #!/usr/bin/env bash
      echo "=== Auto-Rebuild Status ==="
      echo ""
      echo "Path Watcher:"
      if systemctl --user is-active watch-dms-colors.path >/dev/null 2>&1; then
        echo "  ✓ Active"
      else
        echo "  ✗ Inactive"
      fi
      echo ""
      echo "Last Rebuild:"
      systemctl --user status auto-rebuild-niri-theme.service --no-pager -n 5
      echo ""
      echo "Recent Logs:"
      journalctl --user -u auto-rebuild-niri-theme.service -n 10 --no-pager
    '')
  ];

  # Systemd service to automatically rebuild home-manager when colors change
  systemd.user.services.auto-rebuild-niri-theme = {
    Unit = {
      Description = "Automatically rebuild home-manager when DMS colors change";
      After = [
        "graphical-session.target"
        "dankMaterialShell.service"
      ];
    };

    Service = {
      Type = "oneshot";
      ExecStart = "${autoRebuildScript}";

      # Ensure PATH includes necessary binaries
      # Using lib.makeBinPath for proper PATH construction
      Environment = [
        "PATH=${
          lib.makeBinPath (
            with pkgs; [
              nix
              git
              jq
              home-manager
              coreutils
              bash
            ]
          )
        }"
      ];

      # Logging
      StandardOutput = "journal";
      StandardError = "journal";

      # Security - less restrictive since we need git and home access
      PrivateTmp = false;

      # Timeout to prevent hanging
      TimeoutStartSec = "5min";
    };
  };

  # Systemd path watcher to trigger rebuild when DMS colors change
  systemd.user.paths.watch-dms-colors = {
    Unit = {
      Description = "Watch for DMS color changes";
      After = ["graphical-session.target"];
    };

    Path = {
      # Use PathModified instead of PathChanged for more reliable detection
      # PathModified triggers on any write, not just close events
      PathModified = "${config.home.homeDirectory}/.config/DankMaterialShell/dms-colors.json";
      Unit = "auto-rebuild-niri-theme.service";

      # Create the directory if it doesn't exist
      MakeDirectory = true;
      DirectoryMode = "0755";
    };

    Install = {
      WantedBy = ["default.target"];
    };
  };

  # Create reference file explaining the automation
  home.file.".config/niri/AUTO_REBUILD_INFO.md".text = ''
    # Automatic Niri Color Rebuild

    This system automatically updates Niri colors when you change wallpapers in DMS.

    ## How it works

    1. DMS changes wallpaper → Matugen generates colors
    2. Colors saved to ~/.config/DankMaterialShell/dms-colors.json
    3. Systemd path watcher detects the file change
    4. auto-rebuild-niri-theme service triggers
    5. home-manager switch rebuilds configuration
    6. Niri automatically reloads with new colors

    ## Commands

    View colors:
      dms-show-colors

    Manual rebuild:
      dms-trigger-rebuild

    Check status:
      dms-rebuild-status

    Disable auto-rebuild:
      dms-disable-auto-rebuild

    Enable auto-rebuild:
      dms-enable-auto-rebuild

    View logs:
      journalctl --user -u auto-rebuild-niri-theme -f

    ## Performance

    Each rebuild takes 5-30 seconds depending on your hardware.
    Backups are automatically created in ~/.config/niri/backups/

    ## Troubleshooting

    If colors don't update:
    1. Check DMS is running: systemctl --user status dankMaterialShell
    2. Check watcher is active: systemctl --user status watch-dms-colors.path
    3. Check logs: journalctl --user -u auto-rebuild-niri-theme -n 50
    4. Try manual rebuild: dms-trigger-rebuild
    5. Verify flake directory has git: cd ~/.config/home-manager && git status
    6. Make sure files are added to git: cd ~/.config/home-manager && git add .

    ## Git Requirements

    Flakes require all files to be tracked by git. If you get errors about
    "file not found" or "dirty git tree", make sure to:
      cd ~/.config/home-manager
      git add .

    ## Disable if needed

    If rebuilds are too frequent or slow, disable auto-rebuild and manually
    rebuild when desired:

      dms-disable-auto-rebuild
      # Change wallpapers as much as you want
      # When ready:
      dms-trigger-rebuild
  '';

  # Ensure DMS config directory exists
  home.activation.setupDmsDirectories = lib.hm.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD mkdir -p ${config.home.homeDirectory}/.config/DankMaterialShell
    $DRY_RUN_CMD mkdir -p ${config.home.homeDirectory}/.config/niri/backups
  '';
}
