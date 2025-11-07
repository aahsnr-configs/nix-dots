{
  inputs,
  pkgs,
  ...
}: {
  imports = [inputs.nix-doom-emacs-unstraightened.hmModule];

  programs.doom-emacs = {
    enable = true;
    emacs = pkgs.emacs-pgtk;
    extraPackages = epkgs: [
      epkgs.treesit-grammars.with-all-grammars
      pkgs.emacs.pkgs.lsp-bridge
    ];
    doomDir = ./doom.d;
    tangleArgs = "--all config.org";
    experimentalFetchTree = true;
  };
  # services.emacs.enable = true;
}
