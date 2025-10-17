{ inputs
, pkgs
, ...
}: {
  imports = [ inputs.nix-doom-emacs-unstraightened.hmModule ];
  services.emacs = {
    enable = true;
    client.enable = true;
    socketActivation.enable = true;
  };

  programs.doom-emacs = {
    enable = true;
    emacs = pkgs.emacs-unstable-pgtk;
    doomDir = ./doom.d;
    experimentalFetchTree = true;
  };
}
