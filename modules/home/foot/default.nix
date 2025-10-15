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
        pad = "10x10";
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
        alpha = 0.98;
        # cursor = "f0c6c6 f0c6c6";
        # foreground = "c0caf5";
        # background = "1a1b26";
        # selection-foreground = "c0caf5";
        # selection-background = "283457";
        # urls = "73daca";
        #
        # # Regular colors
        # regular0 = "15161e";
        # regular1 = "f7768e";
        # regular2 = "9ece6a";
        # regular3 = "e0af68";
        # regular4 = "7aa2f7";
        # regular5 = "bb9af7";
        # regular6 = "7dcfff";
        # regular7 = "a9b1d6";
        #
        # # Bright colors
        # bright0 = "414868";
        # bright1 = "f7768e";
        # bright2 = "9ece6a";
        # bright3 = "e0af68";
        # bright4 = "7aa2f7";
        # bright5 = "bb9af7";
        # bright6 = "7dcfff";
        # bright7 = "c0caf5";
        #
        # # Extended colors
        # "16" = "ff9e64";
        # "17" = "db4b4b";
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
