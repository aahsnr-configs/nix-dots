{pkgs, ...}: {
  home.packages = [
    (pkgs.emacsWithPackagesFromUsePackage {
      config = ./config.org;
      defaultInitFile = true;
      package = pkgs.emacs-unstable-pgtk;
      alwaysEnsure = false;
      alwaysTangle = true;
      extraEmacsPackages = epkgs:
        with epkgs; [
          use-package
          vterm
          jupyter
          treesit-grammars.with-all-grammars
        ];
      # Override specific package derivations
      # Use this to apply patches, change versions, etc.
      override = final: prev: {
        org = null;
      };
    })
  ];

  # services.emacs = {
  #   enable = true;
  #   client.enable = true;
  #   socketActivation.enable = true;
  # };
}
