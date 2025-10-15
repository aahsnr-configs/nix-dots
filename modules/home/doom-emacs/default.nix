{ inputs
, pkgs
, ...
}: {
  imports = [ inputs.nix-doom-emacs-unstraightened.hmModule ];

  programs.doom-emacs = {
    enable = true;
    emacs = pkgs.emacs-unstable-pgtk;
    doomDir = ./doom.d;
    #tangleArgs = "--all config.org";
    experimentalFetchTree = true;
    extraPackages = epkgs: [ epkgs.treesit-grammars.with-all-grammars ];
  };

  # services.emacs = {
  #   enable = true;
  #   client = {
  #     enable = true;
  #     arguments = [ "-c -a 'emacs'" ];
  #   };
  #   socketActivation.enable = true;
  # };
}
