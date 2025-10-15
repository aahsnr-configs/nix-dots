{
  config,
  pkgs,
  lib,
  ...
}:

let
  # Custom package: example of packaging an external Emacs package
  # This demonstrates packaging lambda-line from GitHub using the idiomatic pattern
  # from emacs-overlay (using trivialBuild)
  lambda-line = pkgs.emacsPackages.trivialBuild rec {
    pname = "lambda-line";
    version = "2024-01-15";

    src = pkgs.fetchFromGitHub {
      owner = "Lambda-Emacs";
      repo = "lambda-line";
      rev = "22186321a7442f1bd3b121f739007bd809cb38f8";
      sha256 = "sha256-2tOXMqpmd14ohzmrRoV5Urf0HlnRPV1EVHm/d8OBSGE=";
    };

    # Elisp dependencies
    propagatedUserEnvPkgs = with pkgs.emacsPackages; [
      all-the-icons
    ];

    buildInputs = propagatedUserEnvPkgs;

    meta = with lib; {
      description = "A minimal modeline for Emacs";
      homepage = "https://github.com/Lambda-Emacs/lambda-line";
      license = licenses.gpl3Plus;
      platforms = platforms.all;
    };
  };

  # Example of a more complex custom package with dependencies
  # Demonstrates building from a subdirectory and handling build inputs
  nano-theme = pkgs.emacsPackages.trivialBuild rec {
    pname = "nano-theme";
    version = "0.3.4";

    src = pkgs.fetchFromGitHub {
      owner = "rougier";
      repo = "nano-theme";
      rev = "87a0a9259d531c7e4bbf7d946b0e896fa87261d4";
      sha256 = "sha256-4t3X9zilGKlYN2BRKE3gZiykf04HQQJ2PzCzUWRyiTE=";
    };

    meta = with lib; {
      description = "A consistent and elegant theme for Emacs";
      homepage = "https://github.com/rougier/nano-theme";
      license = licenses.gpl3Plus;
      platforms = platforms.all;
    };
  };

in
{
  # Enable the Emacs program module
  programs.emacs = {
    enable = true;

    # Select modern natively-compiled Emacs with PGTK (Wayland) support
    #
    # IMPORTANT: Package selection depends on whether emacs-overlay is installed:
    #
    # WITH emacs-overlay:
    #   - pkgs.emacs-git (latest master, most unstable)
    #   - pkgs.emacs-unstable (latest release tag)
    #   - pkgs.emacs-pgtk (PGTK build from overlay)
    #
    # WITHOUT emacs-overlay (standard nixpkgs):
    #   - pkgs.emacs (lucid toolkit)
    #   - pkgs.emacs-gtk (GTK toolkit, can be unstable in daemon mode)
    #   - pkgs.emacs29-pgtk (Emacs 29 with PGTK, if available)
    #
    # The following uses a conditional to work with or without the overlay
    package =
      if builtins.hasAttr "emacs-pgtk" pkgs then
        pkgs.emacs-pgtk # From emacs-overlay
      else if builtins.hasAttr "emacs29-pgtk" pkgs then
        pkgs.emacs29-pgtk # From nixpkgs (Emacs 29+)
      else
        pkgs.emacs-gtk; # Fallback to standard GTK version

    # Declarative package management - single source of truth
    # All Emacs packages are managed here, not in init.el
    extraPackages =
      epkgs: with epkgs; [
        # === Core Utilities ===
        use-package # Package configuration macro
        diminish # Hide minor modes from modeline
        bind-key # Key binding utilities
        general # Convenient key definitions

        # === Completion Framework ===
        vertico # Vertical completion UI
        orderless # Flexible completion style
        marginalia # Rich annotations in minibuffer
        consult # Consulting completing-read
        embark # Contextual actions
        embark-consult # Embark integration with consult
        corfu # Completion overlay (alternative to company)
        cape # Completion-at-point extensions

        # === Version Control ===
        magit # Git porcelain
        diff-hl # Highlight uncommitted changes
        git-timemachine # Step through git history
        forge # Work with git forges (GitHub, GitLab, etc.)

        # === Project Management ===
        projectile # Project interaction library

        # === Programming Support ===
        # Note: eglot is built-in to Emacs 29+
        company # Text completion framework (if not using corfu)
        flycheck # Syntax checking
        flycheck-inline # Display flycheck errors inline
        yasnippet # Template system
        yasnippet-snippets # Snippet collection

        # === Language Modes ===
        nix-mode # Nix expression editing
        nix-ts-mode # Nix mode with tree-sitter
        markdown-mode # Markdown support
        yaml-mode # YAML support
        json-mode # JSON support
        lua-mode # Lua support
        rust-mode # Rust support
        haskell-mode # Haskell support
        python-mode # Python support (in addition to built-in)

        # === Tree-sitter Support ===
        # Install all tree-sitter grammars for enhanced syntax highlighting
        # and code navigation. This provides grammars for all supported languages.
        treesit-grammars.with-all-grammars

        # === UI Enhancements ===
        doom-themes # Theme collection
        doom-modeline # Modern modeline
        all-the-icons # Icon fonts (required by doom-modeline)
        nerd-icons # Alternative icon set
        which-key # Display available keybindings
        rainbow-delimiters # Colorize nested delimiters
        highlight-indent-guides # Visual indent guides

        # === Navigation & Editing ===
        avy # Jump to visible text
        ace-window # Quick window switching
        expand-region # Expand selection by semantic units
        multiple-cursors # Multiple cursor support
        undo-tree # Visualize undo history

        # === Org Mode Extensions ===
        org-modern # Modern org-mode styling
        org-roam # Note-taking system
        org-super-agenda # Organize agenda views
        org-appear # Toggle org element visibility

        # === Utility Packages ===
        helpful # Better help buffers
        wgrep # Writable grep buffers
        exec-path-from-shell # Import environment on macOS
        editorconfig # EditorConfig support
        direnv # direnv integration
        envrc # Alternative direnv integration

        # === Custom External Packages ===
        # Examples of packages built from external sources
        lambda-line # Custom modeline (alternative to doom-modeline)
        nano-theme # Elegant minimal theme
      ];

    # Extra configuration injected at the start of init.el
    # This is useful for bootstrap configuration that must happen before
    # your init.el loads, or for setting up package directories
    extraConfig = ''
      ;; -*- lexical-binding: t; -*-
      ;; This is injected before your init.el by Home Manager

      ;; Disable package.el since we're using Nix for package management
      (setq package-enable-at-startup nil)

      ;; Ensure Nix-installed packages are on the load-path
      ;; Home Manager handles this automatically, but we make it explicit
      (eval-and-compile
        (setq load-prefer-newer t))
    '';

    # Override Emacs package if needed (for patches, custom builds, etc.)
    # Uncomment and modify if you need custom patches or build options
    # overrides = self: super: {
    #   # Example: Override a specific package
    #   # magit = super.magit.overrideAttrs (old: {
    #   #   patches = (old.patches or []) ++ [ ./my-magit.patch ];
    #   # });
    # };
  };

  # Link the user-provided init.el to the correct location
  # This is the recommended approach for integrating existing Emacs configurations
  # The init.el should be in the same directory as this module
  home.file.".config/emacs/init.el" = lib.mkIf (builtins.pathExists ./init.el) {
    source = ./init.el;

    # Optional: Reload init file when it changes
    # Uncomment to automatically reload Emacs configuration on rebuild
    # onChange = ''
    #   if [ -n "$(pgrep -f 'emacs.*daemon')" ]; then
    #     ${pkgs.emacs}/bin/emacsclient -e '(load-file user-init-file)' || true
    #   fi
    # '';
  };

  # Link early-init.el if present
  # Early init runs before package initialization and is useful for:
  # - Setting up package archives (not needed with Nix)
  # - Configuring native compilation
  # - Performance optimizations
  # - UI tweaks that should happen before frame creation
  home.file.".config/emacs/early-init.el" = lib.mkIf (builtins.pathExists ./early-init.el) {
    source = ./early-init.el;
  };

  # Create a custom.el file that Emacs can write to
  # This prevents Emacs from modifying your init.el with custom-set-variables
  home.file.".config/emacs/custom.el" = {
    text = ''
      ;;; custom.el --- Custom variables set by Emacs -*- lexical-binding: t; -*-
      ;;
      ;; This file is managed by Home Manager but Emacs can write to it.
      ;; Custom variables set through Emacs' customize interface will be saved here.
      ;;
      ;;; Code:

      ;; Custom variables will appear below this line

      (provide 'custom)
      ;;; custom.el ends here
    '';
    # Don't replace this file if it already exists (let Emacs manage it)
    force = false;
  };

  # Optional: Enable Emacs daemon for faster startup
  # This runs Emacs as a background service
  services.emacs = {
    enable = false; # Set to true to enable the daemon

    # Enable socket activation (systemd will start daemon on first connection)
    # This is more efficient than always running the daemon
    socketActivation.enable = true;

    # Start emacs client from systemd
    client = {
      enable = true;

      # Arguments to pass to emacsclient
      arguments = [ "-c" ]; # Create a new frame
    };

    # Use the same Emacs package configured above
    package = config.programs.emacs.finalPackage;

    # Optional: Start Emacs daemon at login
    # startWithUserSession = true;
  };

  # Set Emacs as the default editor
  home.sessionVariables = {
    EDITOR = "emacsclient -t -a ''"; # Terminal, fallback to new Emacs
    VISUAL = "emacsclient -c -a ''"; # GUI, fallback to new Emacs
    # ALTERNATE_EDITOR = "";  # Explicit empty alternate (starts daemon if needed)
  };

  # Install supporting tools that Emacs packages might need
  home.packages = with pkgs; [
    # === Language Servers for eglot/lsp-mode ===
    nil # Nix language server
    nixd # Alternative Nix language server
    nixpkgs-fmt # Nix formatter
    nodePackages.typescript-language-server # TypeScript/JavaScript LSP
    nodePackages.vscode-langservers-extracted # HTML/CSS/JSON/ESLint LSP
    nodePackages.yaml-language-server # YAML LSP
    nodePackages.bash-language-server # Bash LSP
    lua-language-server # Lua LSP
    rust-analyzer # Rust LSP
    # pyright                                   # Python LSP (alternative: pylsp)
    # haskell-language-server                   # Haskell LSP

    # === Formatters & Linters ===
    nodePackages.prettier # Multi-language formatter
    shfmt # Shell script formatter
    shellcheck # Shell script linter
    black # Python formatter
    # ruff                                      # Fast Python linter
    # stylua                                    # Lua formatter

    # === Spell Checking ===
    # Install aspell with dictionaries
    # flyspell-mode uses this for spell checking
    (aspellWithDicts (
      dicts: with dicts; [
        en
        en-computers
        en-science
      ]
    ))

    # === Search & Navigation Tools ===
    ripgrep # Fast grep (used by many search tools)
    fd # Fast find (used by projectile, etc.)
    fzf # Fuzzy finder

    # === Git Tools ===
    git # Required for magit
    gitAndTools.delta # Better git diffs
    gitAndTools.gh # GitHub CLI (for forge)

    # === Fonts with Icon Support ===
    # For all-the-icons and doom-modeline
    # Run 'M-x all-the-icons-install-fonts' in Emacs after first install
    emacs-all-the-icons-fonts

    # Nerd Fonts for nerd-icons
    (nerdfonts.override { fonts = [ "NerdFontsSymbolsOnly" ]; })

    # === Additional Utilities ===
    graphviz # For org-babel graph rendering
    # texlive.combined.scheme-medium            # For org-mode LaTeX export
    # pandoc                                    # Document converter (for org-mode)
  ];

  # Optional: Create desktop entry for Emacs client
  # This allows launching Emacs from application menus
  xdg.desktopEntries.emacsclient = lib.mkIf config.services.emacs.enable {
    name = "Emacs Client";
    genericName = "Text Editor";
    comment = "Edit text";
    exec = "emacsclient -c -a '' %F";
    icon = "emacs";
    type = "Application";
    terminal = false;
    categories = [
      "Development"
      "TextEditor"
    ];
    mimeType = [
      "text/english"
      "text/plain"
      "text/x-makefile"
      "text/x-c++hdr"
      "text/x-c++src"
      "text/x-chdr"
      "text/x-csrc"
      "text/x-java"
      "text/x-moc"
      "text/x-pascal"
      "text/x-tcl"
      "text/x-tex"
      "application/x-shellscript"
      "text/x-c"
      "text/x-c++"
    ];
  };

  # Optional: Set up environment variables for better Emacs integration
  home.sessionVariablesExtra = ''
    # Ensure Emacs can find tree-sitter grammars
    # They're installed via Nix in the profile
    if [ -d "$HOME/.nix-profile/lib" ]; then
      export TREE_SITTER_DIR="$HOME/.nix-profile/lib"
    fi
  '';
}
