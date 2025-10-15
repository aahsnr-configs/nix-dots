{ ... }:

{
  programs.foot = {
    enable = true;
    server.enable = true;

    settings = {
      main = {
        shell = "fish";
        title = "foot";
        font = "JetBrainsMono Nerd Font:size=14";
        letter-spacing = 0;
        dpi-aware = "no";
        pad = "8x8";
        bold-text-in-bright = "no";
        gamma-correct-blending = "no";
      };

      scrollback = {
        lines = 10000;
      };

      cursor = {
        style = "beam";
        beam-thickness = 2;
      };

      colors = {
        alpha = 0.95;
      };

      key-bindings = {
        scrollback-up-page = "Page_Up";
        scrollback-down-page = "Page_Down";
        search-start = "Control+Shift+f";
      };

      search-bindings = {
        cancel = "Escape";
        find-prev = "Shift+F3";
        find-next = "F3 Control+G";
      };
    };
  };
}
