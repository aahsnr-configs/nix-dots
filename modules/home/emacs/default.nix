{
  config,
  pkgs,
  lib,
  ...
}:

# let
#   # TODO: Install Combobulate using the method below

#   # Custom package: example of packaging an external Emacs package
#   # This demonstrates packaging lambda-line from GitHub using the idiomatic pattern
#   # from emacs-overlay (using trivialBuild)
#   lambda-line = pkgs.emacsPackages.trivialBuild rec {
#     pname = "lambda-line";
#     version = "2024-01-15";
#
#     src = pkgs.fetchFromGitHub {
#       owner = "Lambda-Emacs";
#       repo = "lambda-line";
#       rev = "22186321a7442f1bd3b121f739007bd809cb38f8";
#       sha256 = "sha256-2tOXMqpmd14ohzmrRoV5Urf0HlnRPV1EVHm/d8OBSGE=";
#     };
#
#     # Elisp dependencies
#     propagatedUserEnvPkgs = with pkgs.emacsPackages; [
#       all-the-icons
#     ];
#
#     buildInputs = propagatedUserEnvPkgs;
#
#     meta = with lib; {
#       description = "A minimal modeline for Emacs";
#       homepage = "https://github.com/Lambda-Emacs/lambda-line";
#       license = licenses.gpl3Plus;
#       platforms = platforms.all;
#     };
#   };
#
#   # Example of a more complex custom package with dependencies
#   # Demonstrates building from a subdirectory and handling build inputs
#   nano-theme = pkgs.emacsPackages.trivialBuild rec {
#     pname = "nano-theme";
#     version = "0.3.4";
#
#     src = pkgs.fetchFromGitHub {
#       owner = "rougier";
#       repo = "nano-theme";
#       rev = "87a0a9259d531c7e4bbf7d946b0e896fa87261d4";
#       sha256 = "sha256-4t3X9zilGKlYN2BRKE3gZiykf04HQQJ2PzCzUWRyiTE=";
#     };
#
#     meta = with lib; {
#       description = "A consistent and elegant theme for Emacs";
#       homepage = "https://github.com/rougier/nano-theme";
#       license = licenses.gpl3Plus;
#       platforms = platforms.all;
#     };
#   };
#
# in
{
  # Enable the Emacs program module
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-unstable-pgtk;
    # All Emacs packages are managed here, not in init.el
    extraPackages =
      epkgs: with epkgs; [
        use-package
        gcmh
        general
        doom-themes
        solaire-mode
        nerd-icons
        doom-modeline
        dashboard
        which-key
        undo-fu
        undo-fu-session
        evil
        evil-collection
        evil-surround
        evil-nerd-commenter
        evil-numbers
        evil-args
        evil-anzu
        evil-exchange
        evil-indent-plus
        evil-visualstar
        evil-matchit
        evil-snipe
        evil-lion
        evil-multiedit
        evil-goggles
        evil-escape
        smartparens
        rainbow-delimiters
        rainbow-mode
        buffer-terminator
        inhibit-mouse
        shackle
        helpful
        wgrep
        indent-bars
        jinx
        orderless
        vertico
        marginalia
        nerd-icons-completion
        consult
        embark
        embark-consult
        corfu
        nerd-icons-corfu
        cape
        org
        org-modern
        org-appear
        org-roam
        org-roam-ui
        consult-org-roam
        evil-org
        fd-dired
        dired-open
        dired-x
        dired-git-info
        nerd-icons-dired
        dired-ranger
        neotree
        envrc
        eglot
        eglot-booster
        eldoc-box
        consult-eglot
        consult-eglot-embark
        dape
        flymake
        flymake-collection
        sideline-flymake
        apheleia
        treesit-fold
        treesit-grammars.with-all-grammars
        evil-textobj-tree-sitter
        jupyter
        magit
        forge
        magit-todos
        git-timemachine
        git-gutter
        pdf-tools
        org-noter
        yasnippet-snippets
        consult-yasnippet
        yasnippet-capf
        auctex
        evil-tex
        citar
        citar-org-roam
        citar-embark
        math-symbol-lists
        cdlatex
        laas
        highlight-indent-guides
      ];

    # Override Emacs package if needed (for patches, custom builds, etc.)
    # Uncomment and modify if you need custom patches or build options
    # overrides = self: super: {
    #   # Example: Override a specific package
    #   # magit = super.magit.overrideAttrs (old: {
    #   #   patches = (old.patches or []) ++ [ ./my-magit.patch ];
    #   # });
    # };
  };

  home.file.".config/emacs/init.el" = lib.mkIf (builtins.pathExists ./init.el) {
    source = ./init.el;

    # Optional: Reload init file when it changes
    # Automatically reload Emacs configuration on rebuild
    onChange = ''
      if [ -n "$(pgrep -f 'emacs.*daemon')" ]; then
        ${pkgs.emacs}/bin/emacsclient -e '(load-file user-init-file)' || true
      fi
    '';
  };

  home.file.".config/emacs/early-init.el" = lib.mkIf (builtins.pathExists ./early-init.el) {
    source = ./early-init.el;
  };

  # Create a custom.el file that Emacs can write to
  # This prevents Emacs from modifying your init.el with custom-set-variables
  home.file.".config/emacs/custom.el" = {
    text = ''
      ;;; custom.el --- Custom variables set by Emacs -*- lexical-binding: t; -*-
      ;;; Code:

      ;; Custom variables will appear below this line

      (provide 'custom)
      ;;; custom.el ends here
    '';
    # Don't replace this file if it already exists (let Emacs manage it)
    force = false;
  };

  services.emacs = {
    enable = false;
    socketActivation.enable = true;
    client = {
      enable = true;
      arguments = [ "-c" ];
    };
    package = config.programs.emacs.finalPackage;

    # NOTE: Is this useful;
    # Optional: Start Emacs daemon at login
    # startWithUserSession = true;
  };

  # Set Emacs as the default editor
  home.sessionVariables = {
    EDITOR = "emacsclient -t -a ''";
    VISUAL = "emacsclient -c -a ''";
  };

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
    if [ -d "$HOME/.nix-profile/lib" ]; then
      export TREE_SITTER_DIR="$HOME/.nix-profile/lib"
    fi
  '';
}
