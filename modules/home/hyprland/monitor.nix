{ ... }:
{
  wayland.windowManager.hyprland.settings = {
    monitorv2 = {
      output = "HDMI-A-1";
      mode = "3840x2160@60";
      position = "0x0";
      scale = 1.67;
      bitdepth = 8;
      supports_hdr = true;
      supports_wide_color = true;
    };
    experimental = {
      xx_color_management_v4 = true;
    };
  };
}
