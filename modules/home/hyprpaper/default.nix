{...}: {
  services.hyprpaper = {
    enable = true;
    settings = {
      ipc = "on";
      splash = false;
      splash_offset = 2.0;

      preload =
        [ "/home/ahsan/Pictures/Wallpapers/black-hole.png" ];

      wallpaper = [
        "HDMI-A-1,/home/ahsan/Pictures/Wallpapers/black-hole.png"
      ];
    };
  };
}
