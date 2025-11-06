# ~/nix-dots/modules/home/yazi/default.nix
{pkgs, ...}: {
  programs.yazi = {
    enable = true;
    package = pkgs.yazi;
    enableFishIntegration = true;
    extraPackages = with pkgs; [
      rich-cli
      ouch
      poppler-utils
    ];

    # Enable desired plugins
    plugins = with pkgs.yaziPlugins; {
      inherit
        full-border
        toggle-pane
        smart-enter
        chmod
        rich-preview
        ouch
        jump-to-char
        git
        lazygit
        starship
        yatline
        ;
    };

    initLua = ''
      require("full-border"):setup {
      	-- Available values: ui.Border.PLAIN, ui.Border.ROUNDED
      	type = ui.Border.ROUNDED,
      }

      require("yatline"):setup({
      	--theme = my_theme,
      	section_separator = { open = "", close = "" },
      	part_separator = { open = "", close = "" },
      	inverse_separator = { open = "", close = "" },

      	style_a = {
      		fg = "black",
      		bg_mode = {
      			normal = "white",
      			select = "brightyellow",
      			un_set = "brightred"
      		}
      	},
      	style_b = { bg = "brightblack", fg = "brightwhite" },
      	style_c = { bg = "black", fg = "brightwhite" },

      	permissions_t_fg = "green",
      	permissions_r_fg = "yellow",
      	permissions_w_fg = "red",
      	permissions_x_fg = "cyan",
      	permissions_s_fg = "white",

      	tab_width = 20,
      	tab_use_inverse = false,

      	selected = { icon = "󰻭", fg = "yellow" },
      	copied = { icon = "", fg = "green" },
      	cut = { icon = "", fg = "red" },

      	total = { icon = "󰮍", fg = "yellow" },
      	succ = { icon = "", fg = "green" },
      	fail = { icon = "", fg = "red" },
      	found = { icon = "󰮕", fg = "blue" },
      	processed = { icon = "󰐍", fg = "green" },

      	show_background = true,

      	display_header_line = true,
      	display_status_line = true,

      	component_positions = { "header", "tab", "status" },

      	header_line = {
      		left = {
      			section_a = {
              			{type = "line", custom = false, name = "tabs", params = {"left"}},
      			},
      			section_b = {
      			},
      			section_c = {
      			}
      		},
      		right = {
      			section_a = {
              			{type = "string", custom = false, name = "date", params = {"%A, %d %B %Y"}},
      			},
      			section_b = {
              			{type = "string", custom = false, name = "date", params = {"%X"}},
      			},
      			section_c = {
      			}
      		}
      	},

      	status_line = {
      		left = {
      			section_a = {
              			{type = "string", custom = false, name = "tab_mode"},
      			},
      			section_b = {
              			{type = "string", custom = false, name = "hovered_size"},
      			},
      			section_c = {
              			{type = "string", custom = false, name = "hovered_path"},
              			{type = "coloreds", custom = false, name = "count"},
      			}
      		},
      		right = {
      			section_a = {
              			{type = "string", custom = false, name = "cursor_position"},
      			},
      			section_b = {
              			{type = "string", custom = false, name = "cursor_percentage"},
      			},
      			section_c = {
              			{type = "string", custom = false, name = "hovered_file_extension", params = {true}},
              			{type = "coloreds", custom = false, name = "permissions"},
      			}
      		}
      	},
      })

    '';

    settings = {
      mgr = {
        show_hidden = true;
        sort_by = "natural";
        sort_dir_first = true;
        linemode = "size";
      };

      # Define how to open different file types
      opener = {
        edit = [
          {
            run = ''nvim "$@"'';
            block = true; # Wait for nvim to close
          }
        ];
        image = [{run = ''imv "$@"'';}];
        video = [
          {
            run = ''mpv "$@"'';
            block = false; # Do not block yazi
            orphan = true; # Detach the mpv process
          }
        ];
        audio = [{run = ''mpv "$@"'';}];
        document = [{run = ''${pkgs.zathura}/bin/zathura "$@"'';}];
        archive = [{run = ''${pkgs.file-roller}/bin/file-roller "$@"'';}];
        fallback = [{run = ''${pkgs.xdg-utils}/bin/xdg-open "$@"'';}];
      };

      # Rules to associate file types with openers
      open.rules = [
        {
          name = "*/";
          use = "edit";
        }
        {
          mime = "text/*";
          use = "edit";
        }
        {
          mime = "image/*";
          use = "image";
        }
        {
          mime = "video/*";
          use = "video";
        }
        {
          mime = "audio/*";
          use = "audio";
        }
        {
          mime = "application/pdf";
          use = "document";
        }
        {
          mime = "application/*zip";
          use = "archive";
        }
        {
          mime = "application/x-tar";
          use = "archive";
        }
        {
          name = "*";
          use = "fallback";
        }
      ];
    };

    # Custom keybindings
    keymap = {
      # FIXED: Renamed 'manager' to 'mgr'
      mgr.prepend = [
        # Navigation
        {
          on = "h";
          run = "leave";
          desc = "Go back";
        }
        {
          on = "j";
          run = "arrow 1";
          desc = "Move down";
        }
        {
          on = "k";
          run = "arrow -1";
          desc = "Move up";
        }
        {
          on = "l";
          run = "enter";
          desc = "Enter";
        }
        {
          on = "G";
          run = "arrow bot";
          desc = "Move to bottom";
        }

        # File operations
        {
          on = "y";
          run = "yank";
          desc = "Copy";
        }
        {
          on = "d";
          run = "yank --cut";
          desc = "Cut";
        }
        {
          on = "p";
          run = "paste";
          desc = "Paste";
        }
        {
          on = [
            "d"
            "d"
          ];
          run = "remove";
          desc = "Remove";
        }
        {
          on = "a";
          run = "create";
          desc = "Create";
        }
        {
          on = "r";
          run = "rename";
          desc = "Rename";
        }

        # Tabs
        {
          on = "t";
          run = "tab_create --current";
          desc = "New tab";
        }
        {
          on = "w";
          run = "tab_close";
          desc = "Close tab";
        }
        {
          on = "[";
          run = "tab_switch -1 --relative";
          desc = "Previous tab";
        }
        {
          on = "]";
          run = "tab_switch 1 --relative";
          desc = "Next tab";
        }

        # Miscellaneous
        {
          on = "q";
          run = "quit";
          desc = "Quit";
        }
        {
          on = ":";
          run = "shell --interactive";
          desc = "Shell";
        }
        {
          on = "!";
          run = "shell --block";
          desc = "Shell (blocking)";
        }
        {
          on = "?";
          run = "help";
          desc = "Help";
        }
        {
          on = "<C-h>";
          run = "hidden toggle";
          desc = "Toggle hidden";
        }
        {
          on = "<C-z>";
          run = "suspend";
          desc = "Suspend";
        }
      ];
    };
  };
}
