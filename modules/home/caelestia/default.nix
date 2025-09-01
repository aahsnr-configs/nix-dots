{ inputs, ... }:

{
  imports = [ inputs.caelestia-shell.homeManagerModules.default ];

  programs.caelestia = {
    enable = true;

    caelestia.settings = {
      appearance = {
        font.family = {
          sans = "Inter";
          mono = "JetBrainsMono Nerd Font";
        };
        rounding.scale = 1.2;
      };

      bar = {
        persistent = true;
        workspaces.shown = 10;
      };

      paths.wallpaperDir = "~/Pictures/Wallpapers";

      services = {
        # For Wayland, this is often not needed.
        gpuType = "";
        weatherLocation = "Dhaka"; # Set your city for the weather widget
        useTwelveHourClock = true;
      };

      launcher = {
        maxShown = 10;
        useFuzzy.apps = true; # Enable fuzzy searching for apps
      };
    };
  };
}
