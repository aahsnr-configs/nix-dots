{ ... }:
{
  services.udiskie = {
    enable = true;
    tray = "always";
    settings = {
      program_options = {
        udisks_version = 2;
        tray = true;
      };
    };
  };
}
