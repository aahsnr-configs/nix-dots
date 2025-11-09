{
  pkgs,
  lib,
  ...
}: {
  programs.yazi = {
    enable = true;
    package = pkgs.yazi;
    enableFishIntegration = true;

    # External packages needed by plugins and features
    extraPackages = with pkgs; [
      # Preview dependencies
      rich-cli
      ouch
      poppler-utils
      ffmpeg
      jq
      file
      unar
    ];

    # Enable desired plugins (only available ones)
    plugins = with pkgs.yaziPlugins; {
      inherit
        # UI Enhancements
        full-border
        toggle-pane
        yatline
        starship
        # Navigation
        smart-enter
        jump-to-char
        # File Operations
        chmod
        smart-paste
        smart-filter
        # Git Integration
        git
        lazygit
        # Preview Enhancements
        rich-preview
        ouch
        diff
        mime-ext
        ;
    };

    # Initialize plugins with Lua configurations
    initLua = ''
      -- ===========================
      -- Full Border Configuration
      -- ===========================
      require("full-border"):setup {
      	type = ui.Border.ROUNDED,
      }

      -- ===========================
      -- Starship Prompt Configuration
      -- ===========================
      require("starship"):setup({
        hide_flags = false,
        flags_after_prompt = true,
      })

      -- ===========================
      -- Git Plugin Configuration
      -- ===========================
      require("git"):setup({})

      -- ===========================
      -- Yatline Status Line Configuration
      -- ===========================
      require("yatline"):setup({
      	section_separator = { open = "", close = "" },
      	part_separator = { open = "", close = "" },
      	inverse_separator = { open = "", close = "" },

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

      	selected = { icon = "", fg = "yellow" },
      	copied = { icon = "", fg = "green" },
      	cut = { icon = "", fg = "red" },
      	total = { icon = "", fg = "yellow" },
      	succ = { icon = "", fg = "green" },
      	fail = { icon = "", fg = "red" },
      	found = { icon = "", fg = "blue" },
      	processed = { icon = "", fg = "green" },

      	show_background = true,
      	display_header_line = true,
      	display_status_line = true,

      	header_line = {
      		left = {
      			section_a = {
              			{type = "line", custom = false, name = "tabs", params = {"left"}},
      			},
      			section_b = {},
      			section_c = {}
      		},
      		right = {
      			section_a = {
              			{type = "string", custom = false, name = "date", params = {"%A, %d %B %Y"}},
      			},
      			section_b = {
              			{type = "string", custom = false, name = "date", params = {"%X"}},
      			},
      			section_c = {}
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
      # ===========================
      # Manager Settings (using mgr, not manager)
      # ===========================
      mgr = {
        show_hidden = true;
        sort_by = "natural";
        sort_dir_first = true;
        linemode = "size";
        show_symlink = true;
      };

      # ===========================
      # Preview Settings
      # ===========================
      preview = {
        max_width = 1000;
        max_height = 1000;
        cache_dir = "";
      };

      # ===========================
      # Plugin Configuration
      # ===========================
      plugin = {
        prepend_previewers = [
          # Rich preview for text files
          {
            name = "*.md";
            run = "rich-preview";
          }
          {
            name = "*.json";
            run = "rich-preview";
          }
          {
            name = "*.csv";
            run = "rich-preview";
          }
          {
            name = "*.rst";
            run = "rich-preview";
          }
          {
            name = "*.ipynb";
            run = "rich-preview";
          }

          # Archive preview with ouch
          {
            mime = "application/*zip";
            run = "ouch";
          }
          {
            mime = "application/x-tar";
            run = "ouch";
          }
          {
            mime = "application/x-bzip2";
            run = "ouch";
          }
          {
            mime = "application/x-7z-compressed";
            run = "ouch";
          }
          {
            mime = "application/x-rar";
            run = "ouch";
          }
          {
            mime = "application/x-xz";
            run = "ouch";
          }
          {
            mime = "application/x-zstd";
            run = "ouch";
          }
        ];
      };

      # ===========================
      # File Opener Configuration
      # ===========================
      opener = {
        edit = [
          {
            run = ''nvim "$@"'';
            block = true;
            desc = "Edit with Neovim";
          }
        ];
        image = [
          {
            run = ''imv "$@"'';
            desc = "View with imv";
          }
        ];
        video = [
          {
            run = ''mpv "$@"'';
            block = false;
            orphan = true;
            desc = "Play with mpv";
          }
        ];
        audio = [
          {
            run = ''mpv "$@"'';
            desc = "Play with mpv";
          }
        ];
        document = [
          {
            run = ''${pkgs.zathura}/bin/zathura "$@"'';
            desc = "View with Zathura";
          }
        ];
        archive = [
          {
            run = ''${pkgs.file-roller}/bin/file-roller "$@"'';
            desc = "Extract with File Roller";
          }
          {
            run = ''ouch decompress "$@"'';
            desc = "Extract with ouch";
          }
        ];
        fallback = [
          {
            run = ''${pkgs.xdg-utils}/bin/xdg-open "$@"'';
            desc = "Open with default application";
          }
        ];
      };

      # ===========================
      # File Opening Rules
      # ===========================
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
          mime = "application/x-bzip2";
          use = "archive";
        }
        {
          mime = "application/x-7z-compressed";
          use = "archive";
        }
        {
          name = "*";
          use = "fallback";
        }
      ];
    };

    # ===========================
    # Custom Keybindings
    # ===========================
    keymap = {
      mgr.prepend_keymap = [
        # ===========================
        # Multi-key bindings MUST come BEFORE single-key bindings
        # ===========================

        # Git Operations (before single 'g')
        {
          on = ["g" "g"];
          run = "plugin lazygit";
          desc = "Open Lazygit";
        }
        {
          on = ["g" "s"];
          run = "plugin git";
          desc = "Show git status";
        }

        # Pane Management
        {
          on = ["z" "p"];
          run = "plugin toggle-pane --args='parent'";
          desc = "Toggle parent pane";
        }
        {
          on = ["z" "m"];
          run = "plugin toggle-pane --args='max-preview'";
          desc = "Maximize preview pane";
        }

        # File Operations
        {
          on = ["d" "d"];
          run = "remove";
          desc = "Remove";
        }
        {
          on = ["c" "m"];
          run = "plugin chmod";
          desc = "Change permissions";
        }

        # ===========================
        # Navigation
        # ===========================
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
          run = "plugin smart-enter";
          desc = "Smart enter";
        }
        {
          on = "G";
          run = "arrow bot";
          desc = "Move to bottom";
        }
        {
          on = "g";
          run = "arrow top";
          desc = "Move to top";
        }

        # Jump to char
        {
          on = "f";
          run = "plugin jump-to-char";
          desc = "Jump to char";
        }

        # ===========================
        # File Operations
        # ===========================
        {
          on = "y";
          run = "yank";
          desc = "Copy";
        }
        {
          on = "x";
          run = "yank --cut";
          desc = "Cut";
        }
        {
          on = "p";
          run = "plugin smart-paste";
          desc = "Smart paste";
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

        # ===========================
        # Pane Management
        # ===========================
        {
          on = "T";
          run = "plugin toggle-pane --args='preview'";
          desc = "Toggle preview pane";
        }

        # ===========================
        # Searching & Filtering
        # ===========================
        {
          on = "/";
          run = "plugin smart-filter";
          desc = "Smart filter";
        }
        {
          on = "s";
          run = "search fd";
          desc = "Search with fd";
        }
        {
          on = "S";
          run = "search rg";
          desc = "Search with ripgrep";
        }
        # Note: Remove zoxide plugin call - use shell integration instead

        # ===========================
        # Tabs
        # ===========================
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
        {
          on = "1";
          run = "tab_switch 0";
          desc = "Switch to tab 1";
        }
        {
          on = "2";
          run = "tab_switch 1";
          desc = "Switch to tab 2";
        }
        {
          on = "3";
          run = "tab_switch 2";
          desc = "Switch to tab 3";
        }
        {
          on = "4";
          run = "tab_switch 3";
          desc = "Switch to tab 4";
        }
        {
          on = "5";
          run = "tab_switch 4";
          desc = "Switch to tab 5";
        }

        # ===========================
        # Miscellaneous
        # ===========================
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
          desc = "Toggle hidden files";
        }
        {
          on = "<C-z>";
          run = "suspend";
          desc = "Suspend";
        }
        {
          on = "=";
          run = "plugin diff";
          desc = "Diff selected and hovered files";
        }
      ];
    };
  };

  home.activation.yaziPlugins = lib.hm.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD mkdir -p ~/.config/yazi/plugins
    $DRY_RUN_CMD ${pkgs.git}/bin/git -C ~/.config/yazi/plugins/yamb.yazi pull || \
    $DRY_RUN_CMD ${pkgs.git}/bin/git clone https://github.com/h-hg/yamb.yazi.git ~/.config/yazi/plugins/yamb.yazi
  '';
}
