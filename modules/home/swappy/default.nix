{...}: {
  programs.swappy = {
    enable = true;
    settings = {
  Default = {
    auto_save = true;
    line_size = 5;
    paint_mode = "brush";
    save_dir = "$HOME/Pictures/Screenshots/";
    save_filename_format = "swappy-%Y%m%d-%H%M%S.png";
    show_panel = false;
  };
    };
  };
}
