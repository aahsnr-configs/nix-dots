{
  inputs,
  pkgs,
  ...
}:
{
  imports = [ inputs.nix-doom-emacs-unstraightened.hmModule ];

  programs.doom-emacs = {
    enable = true;
    emacs = pkgs.emacs30-pgtk;
    doomDir = ./doom.d;
    tangleArgs = "--all config.org";
    experimentalFetchTree = true; # Disable if there are fetcher issues
  };

  services.emacs = {
    enable = true;
    client = {
      enable = true;
      arguments = [ "-c -a 'emacs'" ];
    };
    socketActivation.enable = true;
  };
}
