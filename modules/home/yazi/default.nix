# ~/nix-dots/modules/home/yazi/default.nix
{pkgs, ...}: let
  # Fetch yamb plugin using fetchFromGitHub (Nix best practice)
  yamb-yazi = pkgs.fetchFromGitHub {
    owner = "h-hg";
    repo = "yamb.yazi";
    rev = "22af0033be18eead7b04c2768767d38ccfbaa05b";
    hash = "sha256-NMxZ8/7HQgs+BsZeH4nEglWsRH2ibAzq7hRSyrtFDTA=";
  };
in {
  programs.yazi = {
    enable = true;
    package = pkgs.yazi;
    enableFishIntegration = true;

    # External packages needed by plugins and features
    extraPackages = with pkgs; [
      # Preview dependencies
      rich-cli # For rich-preview plugin
      ouch # For ouch plugin (archive preview/compression)
      poppler-utils # For PDF preview
      ffmpeg # For video preview
      jq # For JSON processing
      file # For mime-type detection
      unar # For archive extraction
      chafa
    ];

    # Enable desired plugins
    plugins = with pkgs.yaziPlugins; {
      # Packaged plugins from nixpkgs
      inherit
        # UI Enhancements
        full-border # Add full border around Yazi
        toggle-pane # Toggle pane visibility
        yatline # Customizable status and header lines
        starship # Starship prompt integration

        # Navigation
        smart-enter # Smart enter: open files or enter directories
        jump-to-char # Vim-like jump to character

        # File Operations
        chmod # Change file permissions
        smart-paste # Paste into hovered directory or CWD
        smart-filter # Enhanced filtering

        # Git Integration
        git # Git status in linemode
        lazygit # Lazygit integration

        # Preview Enhancements
        rich-preview # Preview markdown, JSON, CSV, etc.
        ouch # Archive preview and compression
        diff
        mime-ext
        ;

      # Custom plugin installed via fetchFromGitHub
      yamb = yamb-yazi;
    };

    # Initialize plugins with Lua configurations
    initLua = ''
      -- ===========================
      -- Full Border Configuration
      -- ===========================
      require("full-border"):setup {
      	-- Available values: ui.Border.PLAIN, ui.Border.ROUNDED
      	type = ui.Border.ROUNDED,
      }

      -- ===========================
      -- Starship Prompt Configuration
      -- ===========================
      require("starship"):setup({
        -- Hide flags for cleaner look with full-width starship themes
        hide_flags = false,
        -- Place flags after the starship prompt
        flags_after_prompt = true,
        -- Custom starship config file (optional)
        -- config_file = "~/.config/starship.toml",
      })

      -- ===========================
      -- Git Plugin Configuration
      -- ===========================
      require("git"):setup({
        -- Use Git linemode for showing git status
      })

      -- ===========================
      -- Yamb Bookmarks Configuration
      -- ===========================
      -- Configure yamb with your preferred bookmarks
      local bookmarks = {}
      local path_sep = package.config:sub(1, 1)
      local home_path = os.getenv("HOME")

      -- Add your custom bookmarks here
      -- Syntax: { tag = "Label", path = "/full/path/", key = "x" }
      table.insert(bookmarks, { tag = "Home", path = home_path .. path_sep, key = "h" })
      table.insert(bookmarks, { tag = "Downloads", path = home_path .. path_sep .. "Downloads" .. path_sep, key = "d" })
      table.insert(bookmarks, { tag = "Documents", path = home_path .. path_sep .. "Documents" .. path_sep, key = "D" })
      table.insert(bookmarks, { tag = "Desktop", path = home_path .. path_sep .. "Desktop" .. path_sep, key = "t" })
      table.insert(bookmarks, { tag = "Config", path = home_path .. path_sep .. ".config" .. path_sep, key = "c" })
      table.insert(bookmarks, { tag = "Projects", path = home_path .. path_sep .. "Projects" .. path_sep, key = "p" })
      table.insert(bookmarks, { tag = "Root", path = path_sep, key = "r" })

      require("yamb"):setup {
        bookmarks = bookmarks,
        jump_notify = true,  -- Show notification when jumping to bookmark
        cli = "fzf",  -- Use fzf for fuzzy finding
        keys = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
        path = home_path .. "/.config/yazi/bookmark",  -- Persistence path
      }

      -- ===========================
      -- Yatline Status Line Configuration
      -- ===========================
      require("yatline"):setup({
      	-- Custom theme (optional, uncomment to use)
      	--theme = my_theme,

      	-- Separators
      	section_separator = { open = "", close = "" },
      	part_separator = { open = "", close = "" },
      	inverse_separator = { open = "", close = "" },

      	-- Style configurations
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

      	-- Permission colors
      	permissions_t_fg = "green",
      	permissions_r_fg = "yellow",
      	permissions_w_fg = "red",
      	permissions_x_fg = "cyan",
      	permissions_s_fg = "white",

      	-- Tab settings
      	tab_width = 20,
      	tab_use_inverse = false,

      	-- Icon configurations
      	selected = { icon = "", fg = "yellow" },
      	copied = { icon = "", fg = "green" },
      	cut = { icon = "", fg = "red" },
      	total = { icon = "", fg = "yellow" },
      	succ = { icon = "", fg = "green" },
      	fail = { icon = "", fg = "red" },
      	found = { icon = "", fg = "blue" },
      	processed = { icon = "", fg = "green" },

      	-- Display settings
      	show_background = true,
      	display_header_line = true,
      	display_status_line = true,

      	-- Component positioning
      	component_positions = { "header", "tab", "status" },

      	-- Header line configuration
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

      	-- Status line configuration
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
      # Manager Settings
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
        cache_dir = ""; # Use system cache
      };

      # ===========================
      # Plugin Configuration
      # ===========================
      plugin = {
        # Configure previewers for different file types
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
        # CRITICAL: Multi-key bindings MUST come BEFORE single-key bindings!
        # This prevents conflicts where single keys override multi-key sequences
        # ===========================

        # ===========================
        # Git Operations (must be before single 'g' binding)
        # ===========================
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

        # ===========================
        # Pane Management (multi-key 'z' sequences)
        # ===========================
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

        # ===========================
        # File Operations (multi-key sequences)
        # ===========================
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
        # YAMB BOOKMARKS
        # All bookmark operations use 'm' prefix
        # ===========================
        {
          on = ["m" "a"];
          run = "plugin yamb --args=save";
          desc = "Save bookmark";
        }
        {
          on = ["m" "g"];
          run = "plugin yamb --args=jump_by_key";
          desc = "Jump to bookmark by key";
        }
        {
          on = ["m" "G"];
          run = "plugin yamb --args=jump_by_fzf";
          desc = "Jump to bookmark by fzf";
        }
        {
          on = ["m" "d"];
          run = "plugin yamb --args=delete_by_key";
          desc = "Delete bookmark by key";
        }
        {
          on = ["m" "D"];
          run = "plugin yamb --args=delete_by_fzf";
          desc = "Delete bookmark by fzf";
        }
        {
          on = ["m" "C"];
          run = "plugin yamb --args=delete_all";
          desc = "Delete all bookmarks";
        }
        {
          on = ["m" "r"];
          run = "plugin yamb --args=rename_by_key";
          desc = "Rename bookmark by key";
        }
        {
          on = ["m" "R"];
          run = "plugin yamb --args=rename_by_fzf";
          desc = "Rename bookmark by fzf";
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
          desc = "Smart enter (open file or enter directory)";
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

        # ===========================
        # Jump to char
        # ===========================
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
}
