# ~/.config/home-manager/bat/default.nix
{ pkgs, ... }: {
  programs.bat = {
    enable = true;
    extraPackages = with pkgs.bat-extras; [ batdiff batman prettybat ];
    config = {
      style = "numbers,changes,header";
      "show-all" = true;
      "italic-text" = "always";
      color = "always";
    };
    # Restore useful, non-Fedora-specific syntax mappings
    extraConfig = ''
      --map-syntax "*.nix:Nix"
      --map-syntax "/etc/systemd/system/*:SystemD"
      --map-syntax "/home/*/.config/systemd/user/*:SystemD"
      --map-syntax "Containerfile:Dockerfile"
      --map-syntax "*.containerfile:Dockerfile"
      --map-syntax "*.log:Log"
    '';
  };
}
