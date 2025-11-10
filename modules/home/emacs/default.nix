{pkgs, ...}: {
  home.packages = [
    (pkgs.emacsWithPackagesFromUsePackage {
      config = ./config.org;
      defaultInitFile = true;
      package = pkgs.emacs-pgtk;
      alwaysEnsure = false;
      alwaysTangle = true;
      extraEmacsPackages = epkgs:
        with epkgs; [
          use-package
          vterm
          adaptive-wrap
          visual-fill-column
          auto-dim-other-buffers
          treesit-grammars.with-all-grammars
          minions
          telephone-line
        ];
      override = final: prev: {
        org = null;
      };
    })
  ];

  services.emacs = {
    enable = true;
    client.enable = true;
  };
}
