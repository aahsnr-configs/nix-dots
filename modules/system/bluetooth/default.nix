{ pkgs, ... }:
{
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = false;
    package = pkgs.bluez-experimental;
    settings = {
      General = {
        Experimental = true;
        FastConnectable = true;
      };
    };
  };
}
