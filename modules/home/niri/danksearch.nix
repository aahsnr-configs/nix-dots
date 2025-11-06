{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

with lib;

let
  cfg = config.programs.danksearch;
  tomlFormat = pkgs.formats.toml { };

  # Optimized default configuration
  defaultConfig = {
    # HTTP server settings
    http_port = 43654;

    # Unix socket for IPC
    socket_path = "/tmp/dsearch.sock";

    # Index storage location
    index_path = "${config.home.homeDirectory}/.local/share/dsearch/index";

    # Logging
    log_level = "info";

    # Indexing configuration (OPTIMIZED)
    indexing = {
      enabled = true;

      # Use CPU core count for optimal performance
      workers = 8; # Adjust to your CPU

      max_depth = 50;
      follow_symlinks = false;
      index_hidden = false;

      # Enable content extraction for full-text search
      extract_content = true;

      # 5MB limit for faster indexing (was 10MB)
      max_content_size = 5242880;
    };

    # Paths to index
    paths = [
      config.home.homeDirectory
    ];

    # Comprehensive exclusion patterns
    exclude_patterns = [
      # Version control
      "*/.git/*"
      "*/.svn/*"
      "*/.hg/*"
      "*/.bzr/*"

      # Caches
      "*/.cache/*"
      "*/Cache/*"
      "*/.npm/*"
      "*/.cargo/registry/*"
      "*/.cargo/git/*"
      "*/.rustup/*"

      # Trash
      "*/.local/share/Trash/*"
      "*/.Trash/*"

      # Build outputs
      "*/node_modules/*"
      "*/target/*"
      "*/build/*"
      "*/dist/*"
      "*/_build/*"
      "*/buck-out/*"
      "*/.next/*"
      "*/.nuxt/*"
      "*/out/*"

      # Python
      "*/__pycache__/*"
      "*/.venv/*"
      "*/venv/*"
      "*/.tox/*"
      "*/.mypy_cache/*"
      "*/.pytest_cache/*"
      "*/.ruff_cache/*"
      "*.pyc"

      # Java/Kotlin
      "*/.gradle/*"
      "*/.m2/*"
      "*.class"

      # Temporary files
      "*/*.tmp"
      "*/*.temp"
      "*/*~"
      "*/*.swp"
      "*/*.swo"
      "*/.*.swp"
      "*/.*.swo"

      # IDE
      "*/.idea/*"
      "*/.vscode/*"
      "*/.vs/*"

      # Nix
      "*/result"
      "*/result-*"
      "*/.direnv/*"

      # Large media/binary directories
      "*/.local/share/Steam/*"
      "*/.wine/*"
      "*/Downloads/*.iso"
      "*/Downloads/*.img"

      # Home-manager generations
      "*/.local/state/nix/profiles/*"
      "*/.local/state/home-manager/*"
    ];

    # File extensions to exclude
    exclude_extensions = [
      ".tmp"
      ".temp"
      ".bak"
      ".log"
      ".pyc"
      ".pyo"
      ".o"
      ".so"
      ".a"
      ".la"
      ".dylib"
      ".class"
      ".jar"
      ".war"
      ".ear"
    ];

    # Directory names to exclude
    exclude_dirs = [
      ".git"
      ".svn"
      ".hg"
      ".bzr"
      "node_modules"
      ".npm"
      ".cache"
      "Cache"
      "__pycache__"
      ".venv"
      "venv"
      ".tox"
      "target"
      "build"
      "dist"
      ".gradle"
      ".m2"
      ".idea"
      ".vscode"
      ".vs"
      "result"
      ".direnv"
    ];

    # Real-time file watcher
    watcher = {
      enabled = true;
      # Increased debounce for better performance
      debounce_ms = 2000;
    };

    # Search behavior
    search = {
      default_limit = 50;
      max_limit = 1000;
      default_fuzziness = 1;
      # Prioritize filename matches
      filename_boost = 3.0;
    };

    # EXIF metadata for images
    exif = {
      enabled = true;
      extensions = [
        ".jpg"
        ".jpeg"
        ".png"
        ".gif"
        ".webp"
        ".tiff"
        ".tif"
        ".heic"
        ".heif"
        ".bmp"
        ".svg"
      ];
    };
  };

in
{
  options.programs.danksearch = {
    enable = mkEnableOption "danksearch filesystem search service";

    package = mkOption {
      type = types.package;
      default = inputs.dsearch.packages.${pkgs.system}.default;
      defaultText = literalExpression "inputs.dsearch.packages.\${pkgs.system}.default";
      description = "The danksearch package to use.";
    };

    settings = mkOption {
      type = tomlFormat.type;
      default = defaultConfig;
      description = ''
        Configuration for danksearch.
        See https://github.com/AvengeMedia/danksearch for options.
      '';
      example = literalExpression ''
        {
          http_port = 43654;
          indexing = {
            workers = 16;
            extract_content = true;
          };
          paths = [ "/home/user" "/data" ];
        }
      '';
    };

    enableSystemdService = mkOption {
      type = types.bool;
      default = true;
      description = "Enable the systemd user service for danksearch.";
    };

    enableFirefoxIntegration = mkOption {
      type = types.bool;
      default = false;
      description = "Add danksearch as a Firefox search engine.";
    };
  };

  config = mkIf cfg.enable {
    # Combine all packages into a single home.packages definition
    home.packages = with pkgs; [
      cfg.package
      jq
      curl
      # Helper scripts
      (writeShellScriptBin "dsearch-status" ''
        #!/usr/bin/env bash
        set -euo pipefail

        echo "=== danksearch Service Status ==="
        systemctl --user status dsearch --no-pager || true
        echo ""

        echo "=== Index Information ==="
        INDEX_DIR="${config.home.homeDirectory}/.local/share/dsearch/index"
        if [ -d "$INDEX_DIR" ]; then
          echo "Location: $INDEX_DIR"
          echo "Size: $(${coreutils}/bin/du -sh "$INDEX_DIR" | ${coreutils}/bin/cut -f1)"
          echo "Files: $(${findutils}/bin/find "$INDEX_DIR" -type f | ${coreutils}/bin/wc -l)"
        else
          echo "Index not yet created"
        fi
        echo ""

        echo "=== API Endpoint ==="
        API_URL="http://localhost:${toString cfg.settings.http_port}"
        echo "URL: $API_URL"
        if ${curl}/bin/curl -s -o /dev/null -w "%{http_code}" "$API_URL/health" 2>/dev/null | ${gnugrep}/bin/grep -q "200"; then
          echo "Status: ✓ API responding"
          
          # Try to get stats
          STATS=$(${curl}/bin/curl -s "$API_URL/api/stats" 2>/dev/null || echo "{}")
          if [ "$STATS" != "{}" ]; then
            echo ""
            echo "=== Index Stats ==="
            echo "$STATS" | ${jq}/bin/jq -r '
              "Documents: \(.documents // "N/A")",
              "Last Updated: \(.last_updated // "N/A")"
            ' 2>/dev/null || echo "Stats not available"
          fi
        else
          echo "Status: ✗ API not responding"
        fi
        echo ""

        echo "=== Recent Logs ==="
        journalctl --user -u dsearch -n 5 --no-pager || true
      '')

      (writeShellScriptBin "dsearch-reindex" ''
        #!/usr/bin/env bash
        set -euo pipefail

        echo "⚠️  This will delete the existing index and reindex all files"
        read -p "Continue? (y/N) " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
          echo "Cancelled"
          exit 0
        fi

        echo "Stopping dsearch service..."
        systemctl --user stop dsearch

        echo "Removing old index..."
        ${coreutils}/bin/rm -rf "${config.home.homeDirectory}/.local/share/dsearch/index"

        echo "Starting dsearch service..."
        systemctl --user start dsearch

        echo ""
        echo "✓ Reindexing started"
        echo "This may take several minutes depending on the number of files."
        echo "Monitor progress with: dsearch-status"
      '')

      (writeShellScriptBin "dsearch-test" ''
        #!/usr/bin/env bash
        set -euo pipefail

        API_URL="http://localhost:${toString cfg.settings.http_port}"

        echo "Testing danksearch API..."
        echo ""

        # Health check
        if ${curl}/bin/curl -sf "$API_URL/health" >/dev/null; then
          echo "✓ Health check passed"
        else
          echo "✗ Health check failed"
          exit 1
        fi

        # Test search
        echo "Testing search..."
        RESULT=$(${curl}/bin/curl -s "$API_URL/api/search?q=test&limit=1")
        if echo "$RESULT" | ${jq}/bin/jq -e '.hits' >/dev/null 2>&1; then
          echo "✓ Search API working"
          echo ""
          echo "Sample result:"
          echo "$RESULT" | ${jq}/bin/jq '.'
        else
          echo "✗ Search API failed"
          exit 1
        fi
      '')

      (writeShellScriptBin "dsearch-query" ''
        #!/usr/bin/env bash
        # Quick search wrapper
        if [ $# -eq 0 ]; then
          echo "Usage: dsearch-query <query> [--json]"
          exit 1
        fi

        QUERY="$1"
        API_URL="http://localhost:${toString cfg.settings.http_port}/api/search"

        if [ "$2" = "--json" ]; then
          ${curl}/bin/curl -s "$API_URL?q=$QUERY" | ${jq}/bin/jq '.'
        else
          ${curl}/bin/curl -s "$API_URL?q=$QUERY" | ${jq}/bin/jq -r '.hits[] | "\(.id)"'
        fi
      '')

      (writeShellScriptBin "dsearch-config-show" ''
        #!/usr/bin/env bash
        echo "Current danksearch configuration:"
        echo "================================="
        ${coreutils}/bin/cat "${config.home.homeDirectory}/.config/dsearch/config.toml"
      '')
    ];

    # Write config file
    xdg.configFile."dsearch/config.toml" = {
      source = tomlFormat.generate "dsearch-config.toml" cfg.settings;
    };

    # Systemd service
    systemd.user.services.dsearch = mkIf cfg.enableSystemdService {
      Unit = {
        Description = "danksearch - Indexed filesystem search service";
        Documentation = "https://github.com/AvengeMedia/danksearch";
        After = [ "graphical-session.target" ];
      };

      Service = {
        Type = "simple";
        ExecStart = "${cfg.package}/bin/dsearch serve";
        Restart = "on-failure";
        RestartSec = 10;

        # Logging
        StandardOutput = "journal";
        StandardError = "journal";

        # Security hardening
        PrivateTmp = true;
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = "read-only";

        # Write permissions
        ReadWritePaths = [
          "${config.home.homeDirectory}/.local/share/dsearch"
          "/tmp"
        ];

        # Resource limits (OPTIMIZED)
        MemoryHigh = "3G";
        MemoryMax = "6G";
        CPUQuota = "200%"; # Allow using 2 CPU cores max
      };

      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };

    # Firefox integration
    home.file.".mozilla/firefox/danksearch.xml" = mkIf cfg.enableFirefoxIntegration {
      text = ''
        <?xml version="1.0" encoding="UTF-8"?>
        <OpenSearchDescription xmlns="http://a9.com/-/spec/opensearch/1.1/">
          <ShortName>danksearch</ShortName>
          <Description>Search local files with danksearch</Description>
          <Url type="text/html" template="http://localhost:${toString cfg.settings.http_port}/search?q={searchTerms}"/>
          <InputEncoding>UTF-8</InputEncoding>
        </OpenSearchDescription>
      '';
    };

    # Create directories
    home.activation.dsearchSetup = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      $DRY_RUN_CMD mkdir -p ${config.home.homeDirectory}/.local/share/dsearch
      $DRY_RUN_CMD mkdir -p ${config.home.homeDirectory}/.config/dsearch
    '';

    # Info file
    home.file.".local/share/dsearch/README.md".text = ''
      # danksearch Configuration

      This is the danksearch indexed file search system.

      ## Configuration

      Config file: ~/.config/dsearch/config.toml
      Index location: ~/.local/share/dsearch/index
      API port: ${toString cfg.settings.http_port}

      ## Commands

      Check status:
        dsearch-status

      Force reindex:
        dsearch-reindex

      Test API:
        dsearch-test

      Quick search:
        dsearch-query "term"

      View config:
        dsearch-config-show

      ## API Usage

      Search:
        curl "http://localhost:${toString cfg.settings.http_port}/api/search?q=query"

      Health:
        curl "http://localhost:${toString cfg.settings.http_port}/health"

      ## Performance

      Workers: ${toString cfg.settings.indexing.workers}
      Max file size: ${toString cfg.settings.indexing.max_content_size} bytes
      Content extraction: ${if cfg.settings.indexing.extract_content then "enabled" else "disabled"}

      ## Troubleshooting

      View logs:
        journalctl --user -u dsearch -f

      Restart service:
        systemctl --user restart dsearch

      Check if service is running:
        systemctl --user is-active dsearch
    '';
  };
}
