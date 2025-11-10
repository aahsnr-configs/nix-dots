{...}: {
  programs.kitty = {
    enable = true;

    # Font Configuration
    font = {
      name = "JetBrainsMono Nerd Font";
      size = 12.5;
    };

    # Shell Integration
    shellIntegration.enableZshIntegration = true;
    enableGitIntegration = true;

    settings = {
      # Font Settings
      disable_ligatures = "never";
      adjust_line_height = 0;
      adjust_column_width = 0;

      # Window Settings
      remember_window_size = "yes";
      initial_window_width = 640;
      initial_window_height = 400;
      window_padding_width = 4;
      window_margin_width = 0;
      single_window_margin_width = 1;
      window_border_width = "1.0pt";
      draw_minimal_borders = true;
      placement_strategy = "center";
      hide_window_decorations = "no";
      window_logo_path = "none";
      window_logo_position = "bottom-right";
      window_logo_alpha = 0.5;
      resize_debounce_time = 0.1;
      resize_draw_strategy = "static";
      resize_in_steps = false;
      visual_window_select_characters = "1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ";
      confirm_os_window_close = 0;

      # Tab Bar Configuration
      # -- managed by dms --
      # tab_bar_edge = "bottom";
      # tab_bar_margin_width = 0.0;
      # tab_bar_margin_height = "0.0 0.0";
      # tab_bar_style = "powerline";
      # tab_bar_align = "left";
      # tab_bar_min_tabs = 2;
      # tab_switch_strategy = "previous";
      # tab_fade = "0.25 0.5 0.75 1";
      # tab_separator = " ┇";
      # tab_powerline_style = "slanted";
      # tab_activity_symbol = "none";
      # tab_title_template =
      #   "{fmt.fg.red}{bell_symbol}{activity_symbol}{fmt.fg.tab}{title}";
      # active_tab_title_template = "none";

      # Background/Opacity
      background_opacity = "0.98";
      background_image = "none";
      background_image_layout = "tiled";
      background_image_linear = false;
      dynamic_background_opacity = false;
      background_tint = "0.0";
      dim_opacity = 0.75;

      # Advanced Settings
      editor = "nvim";
      close_on_child_death = false;
      allow_remote_control = true;
      listen_on = "none";
      update_check_interval = 24;
      startup_session = "none";
      clipboard_control = "write-clipboard write-primary";
      clipboard_max_size = 64;
      allow_hyperlinks = true;
      allow_cloning = "ask";
      clone_source_strategies = "virt_env,conda,env_var,path";
      term = "xterm-kitty";

      # Keyboard Modifier
      kitty_mod = "ctrl+shift";
      clear_all_shortcuts = false;

      # Mouse Actions
      mouse_hide_wait = 3.0;
      focus_follows_mouse = false;
      pointer_shape_when_grabbed = "arrow";
      default_pointer_shape = "beam";
      pointer_shape_when_dragging = "beam";
      mouse_map = ''
        left click ungrabbed mouse_handle_click selection link prompt
        shift+left click grabbed,ungrabbed mouse_handle_click selection link prompt
        ctrl+shift+left release grabbed,ungrabbed mouse_handle_click link
        ctrl+shift+left press grabbed discard_event
        middle release ungrabbed paste_from_selection
        left press ungrabbed mouse_selection normal
        ctrl+alt+left press ungrabbed mouse_selection rectangle
        left doubleclick ungrabbed mouse_selection word
        left tripleclick ungrabbed mouse_selection line
        ctrl+alt+left tripleclick ungrabbed mouse_selection line_from_point
        right press ungrabbed mouse_selection extend
        shift+middle release ungrabbed,grabbed paste_selection
        shift+middle press grabbed discard_event
        shift+left press ungrabbed,grabbed mouse_selection normal
        middle release ungrabbed paste_from_selection
      '';

      # Performance Tuning
      repaint_delay = 10;
      input_delay = 3;
      sync_to_monitor = true;

      # Terminal Bell
      enable_audio_bell = true;
      visual_bell_duration = 0.0;
      visual_bell_color = "none";
      window_alert_on_bell = true;
      bell_on_tab = "🔔 ";
      command_on_bell = "none";
      bell_path = "none";

      # Window Layout
      enabled_layouts = "*";
      window_resize_step_cells = 2;
      window_resize_step_lines = 2;

      # Other Advanced Settings
      copy_on_select = false;
      strip_trailing_spaces = "never";
      select_by_word_characters = "@-./_~?&=%+#";
      click_interval = -1.0;

      # OS Specific
      linux_display_server = "auto";
    };

    keybindings = {
      # Clipboard
      "kitty_mod+c" = "copy_to_clipboard";
      "kitty_mod+v" = "paste_from_clipboard";
      "kitty_mod+s" = "paste_from_selection";
      "shift+insert" = "paste_from_selection";
      "kitty_mod+o" = "pass_selection_to_program";
      # Scrolling
      "kitty_mod+up" = "scroll_line_up";
      "kitty_mod+k" = "scroll_line_up";
      "kitty_mod+down" = "scroll_line_down";
      "kitty_mod+j" = "scroll_line_down";
      "kitty_mod+page_up" = "scroll_page_up";
      "kitty_mod+page_down" = "scroll_page_down";
      "kitty_mod+home" = "scroll_home";
      "kitty_mod+end" = "scroll_end";
      "kitty_mod+z" = "scroll_to_prompt -1";
      "kitty_mod+x" = "scroll_to_prompt 1";
      "kitty_mod+h" = "show_scrollback";
      "kitty_mod+g" = "show_last_command_output";
      # Window management
      "kitty_mod+enter" = "new_window";
      "cmd+enter" = "new_window";
      "kitty_mod+n" = "new_os_window";
      "cmd+n" = "new_os_window";
      "kitty_mod+w" = "close_window";
      "kitty_mod+]" = "next_window";
      "kitty_mod+[" = "previous_window";
      "kitty_mod+f" = "move_window_forward";
      "kitty_mod+b" = "move_window_backward";
      "kitty_mod+`" = "move_window_to_top";
      "kitty_mod+r" = "start_resizing_window";
      "cmd+r" = "start_resizing_window";
      "kitty_mod+1" = "first_window";
      "kitty_mod+2" = "second_window";
      "kitty_mod+3" = "third_window";
      "kitty_mod+4" = "fourth_window";
      "kitty_mod+5" = "fifth_window";
      "kitty_mod+6" = "sixth_window";
      "kitty_mod+7" = "seventh_window";
      "kitty_mod+8" = "eighth_window";
      "kitty_mod+9" = "ninth_window";
      "kitty_mod+0" = "tenth_window";
      # Tab management
      "kitty_mod+right" = "next_tab";
      "shift+cmd+]" = "next_tab";
      "ctrl+tab" = "next_tab";
      "kitty_mod+left" = "previous_tab";
      "shift+cmd+[" = "previous_tab";
      "ctrl+shift+tab" = "previous_tab";
      "kitty_mod+t" = "new_tab";
      "cmd+t" = "new_tab";
      "kitty_mod+q" = "close_tab";
      "cmd+w" = "close_tab";
      "shift+cmd+w" = "close_os_window";
      "kitty_mod+." = "move_tab_forward";
      "kitty_mod+," = "move_tab_backward";
      "kitty_mod+alt+t" = "set_tab_title";
      "shift+cmd+i" = "set_tab_title";
      # Layout management
      "kitty_mod+l" = "next_layout";
      "kitty_mod+alt+s" = "goto_layout stack";
      "kitty_mod+alt+p" = "goto_layout fat";
      "kitty_mod+alt+g" = "goto_layout grid";
      "kitty_mod+alt+h" = "goto_layout horizontal";
      "kitty_mod+alt+v" = "goto_layout vertical";
      # Font sizes
      "kitty_mod+equal" = "change_font_size all +2.0";
      "kitty_mod+plus" = "change_font_size all +2.0";
      "kitty_mod+kp_add" = "change_font_size all +2.0";
      "cmd+plus" = "change_font_size all +2.0";
      "cmd+equal" = "change_font_size all +2.0";
      "kitty_mod+minus" = "change_font_size all -2.0";
      "kitty_mod+kp_subtract" = "change_font_size all -2.0";
      "cmd+minus" = "change_font_size all -2.0";
      "kitty_mod+backspace" = "change_font_size all 0";
      "cmd+0" = "change_font_size all 0";
      # Select and act on visible text
      "kitty_mod+e" = "open_url_with_hints";
      "kitty_mod+p>f" = "kitten hints --type path --program -";
      "kitty_mod+p>shift+f" = "kitten hints --type path";
      "kitty_mod+p>l" = "kitten hints --type line --program -";
      "kitty_mod+p>w" = "kitten hints --type word --program -";
      "kitty_mod+p>h" = "kitten hints --type hash --program -";
      "kitty_mod+p>n" = "kitten hints --type linenum";
      # Miscellaneous
      "kitty_mod+f11" = "toggle_fullscreen";
      "kitty_mod+f10" = "toggle_maximized";
      "kitty_mod+u" = "kitten unicode_input";
      "kitty_mod+f2" = "edit_config_file";
      "kitty_mod+escape" = "kitty_shell window";
      "kitty_mod+a>m" = "set_background_opacity +0.1";
      "kitty_mod+a>l" = "set_background_opacity -0.1";
      "kitty_mod+a>1" = "set_background_opacity 1";
      "kitty_mod+a>d" = "set_background_opacity default";
      "kitty_mod+delete" = "clear_terminal reset active";
      "opt+cmd+r" = "clear_terminal reset active";
    };
    extraConfig = ''
      include dank-tabs.conf
      include dank-theme.conf
    '';
  };
}
