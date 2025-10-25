{ config, pkgs, lib, ...}:
{

  home.packages = [
    (pkgs.emacsWithPackagesFromUsePackage {
      config = ./Emacs.org;
      defaultInitFile = true;
      package = pkgs.emacs-pgtk;
      alwaysEnsure = false;
      alwaysTangle = true;
      extraEmacsPackages = epkgs: with epkgs; [
        use-package vterm jupyter      
      ] ++ [
          pkgs.emacs-lsp-booster
          pkgs.texlab
          pkgs.texpresso
          pkgs.gnuplot
        # pkgs.nodePackages.typescript-language-server  # TypeScript LSP
      ];
      
      # Override specific package derivations
      # Use this to apply patches, change versions, etc.
      override = epkgs: epkgs // {
        # Example: Use a specific version or apply patches
        # somePackage = epkgs.melpaPackages.somePackage.overrideAttrs (old: {
        #   patches = [ ./fix.patch ];
        # });
      };
    })
  ];
}
