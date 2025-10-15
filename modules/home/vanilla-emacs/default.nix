{
  config,
  pkgs,
  ...
}:

{
  # Enable the Emacs program module
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-unstable-pgtk;
    # All Emacs packages are managed here, not in init.el
    extraPackages =
      epkgs: with epkgs; [
        no-littering
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

        # (pkgs.callPackage ./nano-theme.nix {
        #   inherit (pkgs) fetchFromGitHub;
        #   inherit (epkgs) trivialBuild;
        # })
      ];
  };

  home.file.".emacs.d/init.el".source = ./init.el;
  home.file.".emacs.d/early-init.el".source = ./early-init.el;

  services.emacs = {
    enable = true;
    package = config.programs.emacs.finalPackage;
  };

  # home.shellAliases = {
  #   e = "emacsclient -t"; # Terminal Emacs
  #   ec = "emacsclient -c"; # GUI Emacs
  #   emacs = "emacsclient -c -a emacs"; # Fallback to standalone if daemon isn't running
  # };

}
