{ config, pkgs, lib, ...}:
{

  home.packages = [
    (pkgs.emacsWithPackagesFromUsePackage {
      config = ./config.org;
      defaultInitFile = true;
      package = pkgs.emacs-unstable-pgtk;
      alwaysEnsure = false;
      alwaysTangle = true;
      #excludePackages = [ "org" ];
      extraEmacsPackages = epkgs: with epkgs; [
        use-package vterm jupyter      
      ];       
      # Override specific package derivations
      # Use this to apply patches, change versions, etc.
      override = final: prev: {
        org = null;
        # Also handle org-plus-contrib if it exists
        org-plus-contrib = null;
      };
    })
  ];
}
