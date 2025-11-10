{
  inputs,
  pkgs,
  ...
}: {
  imports = [inputs.nvf.homeManagerModules.default];

  programs.nvf = {
    enable = true;

    settings = {
      vim = {
        # Basic vim options
        viAlias = true;
        vimAlias = true;
        syntaxHighlighting = true;

        # Enable line numbers
        lineNumberMode = "relNumber";

        # Enable useful optioss
        preventJunkFiles = true;
        clipboard = {
          providers = {
            wl-copy = {
              enable = true;
              providers = pkgs.wl-clipboard;
            };
          };
        };

        notes.todo-comments.enable = true;

        # Enable treesitter for better syntax highlighting
        treesitter = {
          enable = true;
          fold = true;

          grammars = with pkgs.vimPlugins.nvim-treesitter-parsers; [
            nix
            python
            markdown
            markdown_inline
            lua
            vim
            vimdoc
            bash
            json
            yaml
            toml
          ];
        };

        # LSP Configuration
        lsp = {
          enable = true;

          # Format on save
          formatOnSave = true;

          # LSP keybindings
          mappings = {
            goToDefinition = "gd";
            goToDeclaration = "gD";
            goToType = "gt";
            listReferences = "gr";
            nextDiagnostic = "]d";
            previousDiagnostic = "[d";
            openDiagnosticFloat = "<leader>e";
            hover = "K";
            renameSymbol = "<leader>rn";
            codeAction = "<leader>ca";
          };
        };

        # Language-specific configurations
        languages = {
          # Nix language support
          nix = {
            enable = true;

            lsp = {
              enable = true;
              server = "nil";
              package = pkgs.nil;
            };

            treesitter = {
              enable = true;
            };

            extraDiagnostics = {
              enable = true;
            };

            format = {
              enable = true;
              type = "alejandra";
              package = pkgs.alejandra;
            };
          };

          # Python language support
          python = {
            enable = true;

            lsp = {
              enable = true;
              server = "basedpyright"; # Python LSP server
            };

            treesitter = {
              enable = true;
            };

            format = {
              enable = true;
              type = "ruff"; # Python formatter
            };

            # Enable DAP for debugging
            dap = {
              enable = true;
              debugger = "debugpy";
            };
          };

          # Markdown support
          markdown = {
            enable = true;
            lsp.enable = true;
            treesitter.enable = true;
            format = {
              enable = true;
            };
            extensions.render-markdown-nvim = {
              enable = true;
            };
            # extensions.markview-nvim.enable = true;
          };
        };

        # Autocomplete configuration
        autocomplete = {
          nvim-cmp.enable = true;
        };

        # File tree
        filetree = {
          nvimTree = {
            enable = true;
            openOnSetup = false;

            mappings = {
              toggle = "<leader>e";
              focus = "<leader>o";
              findFile = "<leader>f";
            };

            setupOpts = {
              auto_reload_on_write = true;
              hijack_netrw = true;
              sync_root_with_cwd = true;

              view = {
                width = 30;
                side = "left";
              };

              renderer = {
                highlight_git = true;
                icons = {
                  show = {
                    file = true;
                    folder = true;
                    folder_arrow = true;
                    git = true;
                  };
                };
              };

              filters = {
                dotfiles = false;
                # custom = [ ".git" "node_modules" "__pycache__" ];
              };
            };
          };
        };

        # Telescope for fuzzy finding
        telescope = {
          enable = true;

          mappings = {
            findFiles = "<leader>ff";
            liveGrep = "<leader>fg";
            buffers = "<leader>fb";
            helpTags = "<leader>fh";
            gitCommits = "<leader>gc";
            gitBufferCommits = "<leader>gbc";
            gitBranches = "<leader>gb";
            gitStatus = "<leader>gs";
          };
        };

        # Status line
        statusline = {
          lualine = {
            enable = true;
            theme = "auto";
            # activeSection = {
            #   a = ["mode"];
            #   b = ["filename" "location" "progress"];
            #   c = ["diagnostics"];
            #   x = ["lsp_progress" "filetype" "encoding" "fileformat" "branch"];
            #   y = ["diff"];
            # };
            icons.enable = true;
            # componentSeparator.left = "";
            # componentSeparator.right = "";
            # sectionSeparator.left = "";
            # sectionSeparator.right = "";
            # sections = {
            #   lualine_a = ["mode"];
            #   lualine_b = ["filename" "location" "progress"];
            #   lualine_c = ["diagnostics"];
            #   lualine_x = ["lsp_progress" "filetype" "encoding" "fileformat" "branch"];
            #   lualine_y = ["diff"];
            #   lualine_z = [];
            # };
          };
        };

        # Tab line for buffers
        tabline = {
          nvimBufferline = {
            enable = true;
            mappings = {
              cycleNext = "<Tab>";
              cyclePrevious = "<S-Tab>";
              closeCurrent = "<leader>x";
            };
          };
        };

        # Git integration
        git = {
          enable = true;
          gitsigns = {
            enable = true;
            mappings = {
              nextHunk = "]c";
              previousHunk = "[c";
              stageHunk = "<leader>hs";
              resetHunk = "<leader>hr";
              undoStageHunk = "<leader>hu";
              previewHunk = "<leader>hp";
            };
          };
        };

        # Terminal integration
        terminal = {
          toggleterm = {
            enable = true;
            mappings = {
              open = "<C-\\>";
            };
            setupOpts = {
              direction = "horizontal";
              size = 15;
            };
          };
        };

        # Comments plugin
        comments = {
          comment-nvim = {
            enable = true;
            mappings = {
              toggleCurrentLine = "gcc";
              toggleCurrentBlock = "gbc";
              toggleOpLeaderLine = "gc";
              toggleOpLeaderBlock = "gb";
            };
          };
        };

        # Autopairs
        autopairs = {
          nvim-autopairs = {
            enable = true;
          };
        };

        # Indentation guides
        visuals = {
          nvim-web-devicons = {
            enable = true;
          };

          indent-blankline = {
            enable = true;
            setupOpts = {
              indent = {
                char = "│";
              };
              scope = {
                enabled = true;
                show_start = true;
                show_end = false;
              };
            };
          };

          # Smooth scrolling
          nvim-scrollbar = {
            enable = true;
          };
        };

        # UI improvements
        ui = {
          noice = {
            enable = true;
          };

          borders = {
            enable = true;
            globalStyle = "rounded";
          };
        };

        # Theme
        theme = {
          enable = true;
          name = "catppuccin"; # or "gruvbox", "tokyonight", etc.
          style = "mocha"; # for catppuccin: latte, frappe, macchiato, mocha
        };

        # Snippets
        snippets = {
          luasnip = {
            enable = true;
          };
        };

        # Which-key for keybinding hints
        binds = {
          whichKey = {
            enable = true;
          };
        };

        # Utility plugins
        utility = {
          # vim-surround for surrounding text
          surround = {
            enable = true;
          };

          preview.glow.enable = true;

          # Better diffing
          diffview-nvim = {
            enable = true;
          };
        };

        # Custom keymaps
        maps = {
          normal = {
            # Save file
            "<leader>w" = {
              action = ":w<CR>";
              desc = "Save file";
              silent = true;
            };

            # Quit
            "<leader>q" = {
              action = ":q<CR>";
              desc = "Quit";
              silent = true;
            };

            # Clear search highlight
            "<leader>h" = {
              action = ":nohlsearch<CR>";
              desc = "Clear search highlight";
              silent = true;
            };

            # Split windows
            "<leader>sv" = {
              action = ":vsplit<CR>";
              desc = "Split window vertically";
              silent = true;
            };

            "<leader>sh" = {
              action = ":split<CR>";
              desc = "Split window horizontally";
              silent = true;
            };

            # Navigate between windows
            "<C-h>" = {
              action = "<C-w>h";
              desc = "Move to left window";
              silent = true;
            };

            "<C-j>" = {
              action = "<C-w>j";
              desc = "Move to bottom window";
              silent = true;
            };

            "<C-k>" = {
              action = "<C-w>k";
              desc = "Move to top window";
              silent = true;
            };

            "<C-l>" = {
              action = "<C-w>l";
              desc = "Move to right window";
              silent = true;
            };

            # Resize windows
            "<C-Up>" = {
              action = ":resize +2<CR>";
              desc = "Increase window height";
              silent = true;
            };

            "<C-Down>" = {
              action = ":resize -2<CR>";
              desc = "Decrease window height";
              silent = true;
            };

            "<C-Left>" = {
              action = ":vertical resize -2<CR>";
              desc = "Decrease window width";
              silent = true;
            };

            "<C-Right>" = {
              action = ":vertical resize +2<CR>";
              desc = "Increase window width";
              silent = true;
            };
          };

          visual = {
            # Move selected lines
            "J" = {
              action = ":m '>+1<CR>gv=gv";
              desc = "Move selection down";
              silent = true;
            };

            "K" = {
              action = ":m '<-2<CR>gv=gv";
              desc = "Move selection up";
              silent = true;
            };

            # Better indenting
            "<" = {
              action = "<gv";
              desc = "Indent left";
              silent = true;
            };

            ">" = {
              action = ">gv";
              desc = "Indent right";
              silent = true;
            };
          };

          insert = {
            # Quick escape
            "jk" = {
              action = "<ESC>";
              desc = "Exit insert mode";
              silent = true;
            };
          };
        };

        # Optional: Add custom globals
        globals = {
          # Python settings
          python3_host_prog = "/usr/bin/python3"; # or use Nix path
          mapleader = " ";
          # Disable providers you don't use for faster startup
          loaded_ruby_provider = 0;
          loaded_perl_provider = 0;
          loaded_node_provider = 0;
        };

        # Optional: Add custom options
        options = {
          # Tab settings
          tabstop = 2;
          shiftwidth = 2;
          expandtab = true;
          clipboard = "unnamedplus";

          mouse = "a";
          # UI settings
          number = true;
          relativenumber = false;
          signcolumn = "yes";
          cursorline = true;
          wrap = true;

          # Undo settings
          undofile = true;
          undolevels = 10000;

          # Update time
          updatetime = 200;

          # Completion settings
          completeopt = "menu,menuone,noselect";

          # Split settings
          splitright = true;
          splitbelow = true;

          # Scroll offset
          scrolloff = 8;
          sidescrolloff = 8;
        };
      };
    };
  };
}
