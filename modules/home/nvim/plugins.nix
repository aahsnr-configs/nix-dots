# plugins.nix - Additional plugins for enhanced functionality
{ config, pkgs, lib, ... }:

{
  programs.nixvim = {
    plugins = {
      # Telescope for fuzzy finding
      telescope = {
        enable = true;
        
        extensions = {
          fzf-native = {
            enable = true;
            settings = {
              fuzzy = true;
              override_generic_sorter = true;
              override_file_sorter = true;
              case_mode = "smart_case";
            };
          };
          ui-select.enable = true;
        };

        settings = {
          defaults = {
            prompt_prefix = "   ";
            selection_caret = " ";
            entry_prefix = "  ";
            initial_mode = "insert";
            selection_strategy = "reset";
            sorting_strategy = "ascending";
            layout_strategy = "horizontal";
            layout_config = {
              horizontal = {
                prompt_position = "top";
                preview_width = 0.55;
                results_width = 0.8;
              };
              vertical = {
                mirror = false;
              };
              width = 0.87;
              height = 0.80;
              preview_cutoff = 120;
            };
            file_ignore_patterns = [
              "node_modules"
              ".git/"
              "*.pyc"
              "__pycache__"
              ".pytest_cache"
              ".venv"
              "venv"
            ];
            path_display = [ "truncate" ];
            winblend = 0;
            border = { };
            borderchars = [ "─" "│" "─" "│" "╭" "╮" "╯" "╰" ];
            color_devicons = true;
            set_env = {
              COLORTERM = "truecolor";
            };
          };
          pickers = {
            find_files = {
              theme = "dropdown";
              previewer = false;
            };
          };
        };

        keymaps = {
          "<leader>ff" = {
            action = "find_files";
            options.desc = "Find files";
          };
          "<leader>fg" = {
            action = "live_grep";
            options.desc = "Live grep";
          };
          "<leader>fb" = {
            action = "buffers";
            options.desc = "Find buffers";
          };
          "<leader>fh" = {
            action = "help_tags";
            options.desc = "Help tags";
          };
          "<leader>fr" = {
            action = "oldfiles";
            options.desc = "Recent files";
          };
          "<leader>fc" = {
            action = "grep_string";
            options.desc = "Find string under cursor";
          };
          "<leader>fk" = {
            action = "keymaps";
            options.desc = "Find keymaps";
          };
        };
      };

      # Treesitter for better syntax highlighting
      treesitter = {
        enable = true;
        
        settings = {
          highlight = {
            enable = true;
            additional_vim_regex_highlighting = false;
          };
          
          indent = {
            enable = true;
          };
          
          incremental_selection = {
            enable = true;
            keymaps = {
              init_selection = "<C-space>";
              node_incremental = "<C-space>";
              scope_incremental = false;
              node_decremental = "<bs>";
            };
          };
          
          ensure_installed = [
            "python"
            "nix"
            "markdown"
            "markdown_inline"
            "lua"
            "vim"
            "vimdoc"
            "bash"
            "json"
            "yaml"
            "toml"
            "regex"
          ];
        };

        nixvimInjections = true;
      };

      # Treesitter context for sticky function headers
      treesitter-context = {
        enable = true;
        settings = {
          max_lines = 3;
          min_window_height = 20;
        };
      };

      # Git integration
      gitsigns = {
        enable = true;
        settings = {
          signs = {
            add = {
              text = "│";
            };
            change = {
              text = "│";
            };
            delete = {
              text = "_";
            };
            topdelete = {
              text = "‾";
            };
            changedelete = {
              text = "~";
            };
            untracked = {
              text = "┆";
            };
          };
          current_line_blame = true;
          current_line_blame_opts = {
            virt_text = true;
            virt_text_pos = "eol";
            delay = 500;
          };
          on_attach = ''
            function(bufnr)
              local gs = package.loaded.gitsigns

              local function map(mode, l, r, opts)
                opts = opts or {}
                opts.buffer = bufnr
                vim.keymap.set(mode, l, r, opts)
              end

              -- Navigation
              map('n', ']c', function()
                if vim.wo.diff then return ']c' end
                vim.schedule(function() gs.next_hunk() end)
                return '<Ignore>'
              end, {expr=true, desc="Next hunk"})

              map('n', '[c', function()
                if vim.wo.diff then return '[c' end
                vim.schedule(function() gs.prev_hunk() end)
                return '<Ignore>'
              end, {expr=true, desc="Previous hunk"})

              -- Actions
              map('n', '<leader>gs', gs.stage_hunk, {desc="Stage hunk"})
              map('n', '<leader>gr', gs.reset_hunk, {desc="Reset hunk"})
              map('v', '<leader>gs', function() gs.stage_hunk {vim.fn.line('.'), vim.fn.line('v')} end, {desc="Stage hunk"})
              map('v', '<leader>gr', function() gs.reset_hunk {vim.fn.line('.'), vim.fn.line('v')} end, {desc="Reset hunk"})
              map('n', '<leader>gS', gs.stage_buffer, {desc="Stage buffer"})
              map('n', '<leader>gu', gs.undo_stage_hunk, {desc="Undo stage hunk"})
              map('n', '<leader>gR', gs.reset_buffer, {desc="Reset buffer"})
              map('n', '<leader>gp', gs.preview_hunk, {desc="Preview hunk"})
              map('n', '<leader>gb', function() gs.blame_line{full=true} end, {desc="Blame line"})
              map('n', '<leader>gd', gs.diffthis, {desc="Diff this"})
            end
          '';
        };
      };

      # Lazygit integration
      lazygit = {
        enable = true;
      };

      # Autopairs
      nvim-autopairs = {
        enable = true;
        settings = {
          check_ts = true;
          ts_config = {
            lua = [ "string" "source" ];
            javascript = [ "string" "template_string" ];
          };
          disable_filetype = [ "TelescopePrompt" "spectre_panel" ];
          fast_wrap = {
            map = "<M-e>";
            chars = [ "{" "[" "(" "\"" "'" ];
            pattern = "[%'%\"%)%>%]%)%}%,]";
            offset = 0;
            end_key = "$";
            keys = "qwertyuiopzxcvbnmasdfghjkl";
            check_comma = true;
            highlight = "PmenuSel";
            highlight_grey = "LineNr";
          };
        };
      };

      # Comment toggling
      comment = {
        enable = true;
        settings = {
          opleader = {
            line = "gc";
            block = "gb";
          };
          toggler = {
            line = "gcc";
            block = "gbc";
          };
        };
      };

      # Surround text objects
      nvim-surround = {
        enable = true;
      };

      # Better marks
      marks = {
        enable = true;
        settings = {
          default_mappings = true;
          builtin_marks = [ "." "<" ">" "^" ];
          cyclic = true;
          force_write_shada = false;
          refresh_interval = 250;
          sign_priority = { lower = 10; upper = 15; builtin = 8; bookmark = 20; };
          excluded_filetypes = [ "qf" "NvimTree" "toggleterm" "TelescopePrompt" "alpha" "netrw" ];
          bookmark_0 = {
            sign = "⚑";
            virt_text = "hello world";
          };
          mappings = { };
        };
      };

      # Markdown preview
      markdown-preview = {
        enable = true;
        settings = {
          auto_start = false;
          auto_close = true;
          refresh_slow = false;
          command_for_global = false;
          open_to_the_world = false;
          browser = "";
          echo_preview_url = true;
          port = "8080";
          theme = "dark";
        };
      };

      # Better code folding
      nvim-ufo = {
        enable = true;
        settings = {
          provider_selector = ''
            function(bufnr, filetype, buftype)
              return {'treesitter', 'indent'}
            end
          '';
        };
      };

      # Todo comments highlighting
      todo-comments = {
        enable = true;
        settings = {
          signs = true;
          keywords = {
            FIX = {
              icon = " ";
              color = "error";
              alt = [ "FIXME" "BUG" "FIXIT" "ISSUE" ];
            };
            TODO = {
              icon = " ";
              color = "info";
            };
            HACK = {
              icon = " ";
              color = "warning";
            };
            WARN = {
              icon = " ";
              color = "warning";
              alt = [ "WARNING" "XXX" ];
            };
            PERF = {
              icon = " ";
              alt = [ "OPTIM" "PERFORMANCE" "OPTIMIZE" ];
            };
            NOTE = {
              icon = " ";
              color = "hint";
              alt = [ "INFO" ];
            };
          };
        };
      };

      # Better quickfix/location list
      nvim-bqf = {
        enable = true;
      };

      # Session management
      persistence = {
        enable = true;
      };

      # Better terminal integration
      toggleterm = {
        enable = true;
        settings = {
          direction = "float";
          float_opts = {
            border = "curved";
            winblend = 0;
          };
          open_mapping = "[[<C-\\>]]";
          shade_terminals = true;
          start_in_insert = true;
          persist_size = true;
          close_on_exit = true;
        };
      };

      # File tree
      neo-tree = {
        enable = true;
        closeIfLastWindow = true;
        window = {
          width = 30;
          mappings = {
            "<space>" = "none";
          };
        };
        filesystem = {
          followCurrentFile = {
            enabled = true;
          };
          useLibuvFileWatcher = true;
        };
      };

      # Python virtual environment selector
      venv-selector = {
        enable = true;
        settings = {
          auto_refresh = true;
          search_venv_managers = true;
          search_workspace = true;
          notify_user_on_venv_activation = true;
        };
      };

      # Better UI for vim.ui.select and vim.ui.input
      dressing = {
        enable = true;
        settings = {
          input = {
            enabled = true;
            default_prompt = "➤ ";
            win_options = {
              winblend = 0;
            };
          };
          select = {
            enabled = true;
            backend = [ "telescope" "builtin" ];
          };
        };
      };

      # Smooth scrolling
      nvim-scrollbar = {
        enable = true;
        settings = {
          handle = {
            color = "#3b4261";
          };
          marks = {
            Search = { color = "#ff9e64"; };
            Error = { color = "#f7768e"; };
            Warn = { color = "#e0af68"; };
            Info = { color = "#7aa2f7"; };
            Hint = { color = "#1abc9c"; };
            Misc = { color = "#9d7cd8"; };
          };
        };
      };

      # Illuminate - highlight word under cursor
      illuminate = {
        enable = true;
        underCursor = false;
        filetypesDenylist = [
          "DressingSelect"
          "Outline"
          "TelescopePrompt"
          "alpha"
          "harpoon"
          "reason"
          "neo-tree"
        ];
      };
    };
  };
}
