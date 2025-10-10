{ inputs, pkgs, ... }:

{
  imports = [ inputs.caelestia-shell.homeManagerModules.default ];

  programs.caelestia = {
    enable = true;
    systemd = {
      enable = false;
      target = "graphical-session.target";
      environment = [ ];
    };
    cli = {
      enable = true;
      settings = {
        theme.enableGtk = true;
      };
    };
  };

  home.packages = with pkgs; [
    aubio
    brightnessctl
    cava
    cliphist
    dart-sass
    ddcutil
    fuzzel
    grim
    libnotify
    libpulseaudio
    lm_sensors
    slurp
    swappy
    wf-recorder
    wl-screenrec
    xkeyboard-config
    ydotool
  ];

  home = {
    file.".config/caelestia/hypr-user.conf".source = ./hypr-user.conf;
    file.".config/caelestia/hypr-vars.conf".source = ./hypr-vars.conf;
    file.".config/caelestia/shell.json".source = ./shell.json;
  };

}
