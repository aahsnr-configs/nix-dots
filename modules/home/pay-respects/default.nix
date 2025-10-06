# ~/nix-dots/modules/home/pay-respects/default.nix
{ ... }:
{
  programs.pay-respects = {
    enable = true;
    enableFishIntegration = true;
    options = [
      "--alias"
      "f"
    ];
  };
}
