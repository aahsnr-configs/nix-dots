# ~/nix-dots/modules/home/atuin/default.nix
{
  pkgs,
  inputs,
  ...
}: {
  programs.atuin = {
    enable = true;
    package = pkgs.atuin;
    enableFishIntegration = true;
    daemon = {
      enable = true;
      logLevel = "warn";
    };
    settings = {
      auto_sync = true;
      sync_frequency = "5m";
      sync_address = "https://api.atuin.sh";
      search_mode = "prefix";
    };
  };
}
