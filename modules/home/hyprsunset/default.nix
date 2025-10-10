{ inputs, pkgs, ... }:
{
  services.hyprsunset = {
    enable = true;
    package = inputs.hyprsunset.packages.${pkgs.system}.hyprsunset;
    settings = {
      max-gamma = 150;
      profile = [
        {
          time = "7:30";
          temperature = 5700;
          gamma = 1.0;
        }
        {
          time = "21:00";
          temperature = 5700;
          gamma = 1.0;
        }
      ];
    };
  };
}
