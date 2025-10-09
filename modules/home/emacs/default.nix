{
  pkgs,
  ...
}:

let
  # The emacs-overlay provides a function to parse your emacs config
  # and install the declared packages.
  emacs-config = pkgs.emacsWithPackagesFromUsePackage {
    config = ./init.el;
    # You can choose your preferred emacs version.
    package = pkgs.emacs30-pgtk;
    # `alwaysEnsure` is equivalent to setting `use-package-always-ensure` to `t`.
    alwaysEnsure = true;
    # `alwaysTangle` will tangle all code blocks that do not have a `:tangle`
    # argument. This is useful when you have a `#+PROPERTY: header-args:emacs-lisp :tangle yes`
    # in your org file.
    alwaysTangle = false;
    # You can also add extra packages that are not in your config file.
    extraEmacsPackages = epkgs: [
      epkgs.no-littering
    ];
  };

in
{
  # early-init.el is not needed since all files are managed by nix
  home.packages = emacs-config;

  services.emacs = {
    enable = true;
    client = {
      enable = true;
      arguments = [ "-c -a 'emacs'" ];
    };
    socketActivation.enable = true;
  };
}
