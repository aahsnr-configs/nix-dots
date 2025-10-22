# ui.nix - UI plugins and theme configuration
{ config, pkgs, lib, ... }:

{
  programs.nixvim = {
    # Catppuccin Mocha theme
    colorschemes.catppuccin = {
      enable = true;
      settings = {
        flavour = "mocha";
        transparent_background = false;
        term_colors = true;
        
        # Integration with other plugins
        integrations = {
          cmp = true;
          gitsigns = true;
          treesitter = true;
          telescope = {
            enabled = true;
          };
          native_lsp = {
            enabled = true;
            inlay_hints = {
              background = true;
            };
          };
          which_key = true;
          snacks = true;
        };
        
        # Styling
        styles = {
          comments = [ "italic" ];
          conditionals = [ "italic" ];
          functions = [ "bold" ];
        };
      };
    };

    plugins = {
      # Modern statusline with lualine
      lualine = {
        enable = true;
        settings = {
          options = {
            icons_enabled = true;
            theme = "catppuccin";
            component_separators = {
              left = "|";
              right = "|";
            };
            section_separators = {
              left = "";
              right = "";
            };
            globalstatus = true;
          };
          
          sections = {
            lualine_a = [ "mode" ];
            lualine_b = [ "branch" "diff" ];
            lualine_c = [
              {
                __unkeyed-1 = "filename";
                path = 1; # Relative path
              }
            ];
            lualine_x = [
              {
                __unkeyed-1 = "diagnostics";
                sources = [ "nvim_lsp" ];
                symbols = {
                  error = " ";
                  warn = " ";
                  info = " ";
                  hint = " ";
                };
              }
              "encoding"
              "fileformat"
              "filetype"
            ];
            lualine_y = [ "progress" ];
            lualine_z = [
              "location"
              {
                __unkeyed-1.__raw = ''
                  function()
                    return os.date("%H:%M")
                  end
                '';
              }
            ];
          };
          
          inactive_sections = {
            lualine_a = [ ];
            lualine_b = [ ];
            lualine_c = [ "filename" ];
            lualine_x = [ "location" ];
            lualine_y = [ ];
            lualine_z = [ ];
          };
        };
      };

      # Web devicons for file icons
      web-devicons.enable = true;

      # Snacks for various UI enhancements
      snacks = {
        enable = true;
        settings = {
          # Dashboard
          dashboard = {
            enabled = true;
            preset = {
              header = ''
                ███╗   ██╗██╗██╗  ██╗██╗   ██╗██╗███╗   ███╗
                ████╗  ██║██║╚██╗██╔╝██║   ██║██║████╗ ████║
                ██╔██╗ ██║██║ ╚███╔╝ ██║   ██║██║██╔████╔██║
                ██║╚██╗██║██║ ██╔██╗ ╚██╗ ██╔╝██║██║╚██╔╝██║
                ██║ ╚████║██║██╔╝ ██╗ ╚████╔╝ ██║██║ ╚═╝ ██║
                ╚═╝  ╚═══╝╚═╝╚═╝  ╚═╝  ╚═══╝  ╚═╝╚═╝     ╚═╝
              '';
            };
            sections = [
              { section = "header"; }
              { section = "keys"; gap = 1; padding = 1; }
              { section = "startup"; }
            ];
          };
          
          # Better notifications
          notifier = {
            enabled = true;
            timeout = 3000;
          };
          
          # Status column enhancements
          statuscolumn = {
            enabled = true;
          };
          
          # Better input and select UI
          input = {
            enabled = true;
          };
        };
      };

      # Indent guides
      indent-blankline = {
        enable = true;
        settings = {
          scope = {
            enabled = true;
            show_start = true;
            show_end = false;
          };
        };
      };

      # Highlight colors in code
      nvim-colorizer = {
        enable = true;
        userDefaultOptions = {
          RGB = true;
          RRGGBB = true;
          names = true;
          RRGGBBAA = true;
          rgb_fn = true;
          hsl_fn = true;
          css = true;
          css_fn = true;
        };
      };

      # Better buffer deletion
      bufdelete.enable = true;

      # Which-key for keybinding discovery
      which-key = {
        enable = true;
        settings = {
          delay = 300;
          preset = "modern";
          spec = [
            {
              __unkeyed-1 = "<leader>f";
              group = "Find";
              icon = " ";
            }
            {
              __unkeyed-1 = "<leader>l";
              group = "LSP";
              icon = " ";
            }
            {
              __unkeyed-1 = "<leader>g";
              group = "Git";
              icon = " ";
            }
            {
              __unkeyed-1 = "<leader>b";
              group = "Buffer";
              icon = " ";
            }
            {
              __unkeyed-1 = "<leader>c";
              group = "Code";
              icon = " ";
            }
          ];
        };
      };

      # Noice for better cmd line and messages
      noice = {
        enable = true;
        settings = {
          lsp = {
            override = {
              "vim.lsp.util.convert_input_to_markdown_lines" = true;
              "vim.lsp.util.stylize_markdown" = true;
              "cmp.entry.get_documentation" = true;
            };
          };
          presets = {
            bottom_search = true;
            command_palette = true;
            long_message_to_split = true;
            inc_rename = false;
            lsp_doc_border = true;
          };
        };
      };
    };
  };
}
