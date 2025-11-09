{pkgs, ...}: {
  services.greetd = {
    enable = true;
    settings = {
      terminal = {
        vt = 1;
      };
      default_session = {
        user = "greeter";
        command = "${pkgs.tuigreet}/bin/tuigreet --command niri";
      };
    };
  };
}
