{ pkgs, ... }: {
  # environment.etc."greetd/hyprland.conf".text = ''
  #   exec-once = regreet; hyprctl dispatch exit
  # '';
  #
  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.greetd.tuigreet}/bin/tuigreet --cmd Hyprland";
        user = "greeter";
      };
    };
  };
}
