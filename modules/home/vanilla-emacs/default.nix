{
  config,
  pkgs,
  ...
}:

let
  myEmacs = pkgs.emacsWithPackagesFromUsePackage {
    config = ./init.el;
    defaultInitFile = true;
    package = pkgs.emacs-unstable-pgtk;
    alwaysEnsure = true;
    alwaysTangle = false;
    extraEmacsPackages = epkgs: [
      # Add any packages that aren't declared with use-package
      # or that need to be explicitly included
      epkgs.use-package
      pkgs.shellcheck
      pkgs.ripgrep
      pkgs.fd
    ];

    # Optionally override derivations.
    override = final: prev: {
      # Example: Override a package if needed
      # weechat = prev.melpaPackages.weechat.overrideAttrs(old: {
      #   patches = [ ./weechat-el.patch ];
      # });
    };
  };

in
{
  home.packages = [ myEmacs ];
  # services.emacs = {
  #   enable = true;
  #   package = myEmacs;
  #   client.enable = true;
  #   startWithUserSession = "graphical";
  # };

  # Optional: Add shell aliases for convenient Emacs usage
  home.shellAliases = {
    e = "emacsclient -t";          # Terminal Emacs
    ec = "emacsclient -c";          # GUI Emacs
    emacs = "emacsclient -c -a emacs"; # Fallback to standalone if daemon isn't running
  };
}
