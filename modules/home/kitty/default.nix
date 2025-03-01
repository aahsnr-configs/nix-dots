{ config, pkgs, ... }:

{
  programs.kitty = {
    enable = true;
    font = {
      name = "JetBrainsMono Nerd Font";
      size = 22;
    };
    environment = {
      "tab_bar_edge"          = "bottom";
      "tab_bar_margin_width"  = "0.0";
      "tab_bar_margin_height" = "0.0 0.0";
      "tab_bar_style"         =  "powerline";
      "tab_bar_align"         = "left";
    };
    settings = {
      scrollback_lines = 10000;
      wheel_scroll_min_lines  = 1;
      remember_window_size    = "yes";
      initial_window_width    = 640;
      initial_window_height   = 400;
      window_padding_width    = 4;
      allow_hyperlinks        = "yes";
      confirm_os_window_close = 0;
      enable_audio_bell = false;
    };
    keybindings = {
      "ctrl+shift" = "kitty_mod";
      "ctrl+tab"   = "next_tab";
      "ctrl+shift+tab" = "previous_tab";
      "ctrl+t" = "new_tab";
      "kitty_mod+q" = "close_tab";
    };

    # theme = "Catppuccin-Macchiato";
    shellIntegration = {
      mode = "enabled";
      enableZshIntegration = true;
    };

    extraConfig = ''
      tab_bar_style fade
      tab_fade 1
      active_tab_font_style   bold
      inactive_tab_font_style bold
    '';
  };
}
