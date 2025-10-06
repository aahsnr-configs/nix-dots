{ ... }:
{
  systemd.user.services = {
    turnoffdisplay = {
      enable = true;
      wantedBy = [ "default.target" ];
      description = "Turns off laptop's display after startup";
      serviceConfig = {
        Type = "simple";
        ExecStart = ''
          hyprctl keyword monitor "eDP-1,disable"
        '';
      };
    };
  };
}
