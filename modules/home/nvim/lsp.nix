# lsp.nix - LSP configuration for Python, Nix, and Markdown
{ config, pkgs, lib, ... }:

{
  programs.nixvim = {
    plugins = {
      # LSP configuration
      lsp = {
        enable = true;
        
        # Enable keymaps for LSP actions
        keymaps = {
          silent = true;
          diagnostic = {
            "<leader>ld" = "open_float";
            "[d" = "goto_prev";
            "]d" = "goto_next";
            "<leader>lq" = "setloclist";
          };
          lspBuf = {
            "gd" = "definition";
            "gD" = "declaration";
            "K" = "hover";
            "gi" = "implementation";
            "<C-k>" = "signature_help";
            "<leader>lwa" = "add_workspace_folder";
            "<leader>lwr" = "remove_workspace_folder";
            "<leader>lwl" = {
              action = ''
                function()
                  print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
                end
              '';
            };
            "<leader>lt" = "type_definition";
            "<leader>lr" = "rename";
            "<leader>la" = "code_action";
            "gr" = "references";
            "<leader>lf" = "format";
          };
        };

        # Enable inlay hints, codelens, and semantic tokens
        inlayHints = true;
        
        servers = {
          # Python
          pyright = {
            enable = true;
            settings = {
              python = {
                analysis = {
                  typeCheckingMode = "basic";
                  autoSearchPaths = true;
                  useLibraryCodeForTypes = true;
                  diagnosticMode = "workspace";
                };
              };
            };
          };

          # Nix
          nil-ls = {
            enable = true;
            settings = {
              formatting = {
                command = [ "${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt" ];
              };
            };
          };

          # Markdown
          marksman = {
            enable = true;
          };

          # Bash
          bashls = {
            enable = true;
          };
        };
      };

      # none-ls for additional diagnostics and formatting
      none-ls = {
        enable = true;
        sources = {
          # Python
          diagnostics = {
            ruff.enable = true;
          };
          formatting = {
            black = {
              enable = true;
              settings = ''
                {
                  extra_args = { "--fast" },
                }
              '';
            };
          };

          # Markdown
          diagnostics.markdownlint = {
            enable = true;
          };
        };
      };

      # Conform for formatting on save
      conform-nvim = {
        enable = true;
        settings = {
          format_on_save = {
            lsp_format = "fallback";
            timeout_ms = 500;
          };
          
          formatters_by_ft = {
            python = [ "black" ];
            nix = [ "nixpkgs_fmt" ];
            markdown = [ "prettier" ];
            json = [ "prettier" ];
            yaml = [ "prettier" ];
            lua = [ "stylua" ];
            "_" = [ "trim_whitespace" ];
          };
        };
      };

      # Autocompletion with nvim-cmp
      cmp = {
        enable = true;
        autoEnableSources = true;
        
        settings = {
          snippet = {
            expand = ''
              function(args)
                require('luasnip').lsp_expand(args.body)
              end
            '';
          };

          mapping = {
            __raw = ''
              cmp.mapping.preset.insert({
                ['<C-b>'] = cmp.mapping.scroll_docs(-4),
                ['<C-f>'] = cmp.mapping.scroll_docs(4),
                ['<C-Space>'] = cmp.mapping.complete(),
                ['<C-e>'] = cmp.mapping.abort(),
                ['<CR>'] = cmp.mapping.confirm({ select = true }),
                ['<Tab>'] = cmp.mapping(function(fallback)
                  if cmp.visible() then
                    cmp.select_next_item()
                  elseif require('luasnip').expand_or_jumpable() then
                    require('luasnip').expand_or_jump()
                  else
                    fallback()
                  end
                end, { 'i', 's' }),
                ['<S-Tab>'] = cmp.mapping(function(fallback)
                  if cmp.visible() then
                    cmp.select_prev_item()
                  elseif require('luasnip').jumpable(-1) then
                    require('luasnip').jump(-1)
                  else
                    fallback()
                  end
                end, { 'i', 's' }),
              })
            '';
          };

          sources = [
            { name = "nvim_lsp"; priority = 1000; }
            { name = "luasnip"; priority = 750; }
            { name = "path"; priority = 500; }
            { name = "buffer"; priority = 250; }
          ];

          window = {
            completion = {
              border = "rounded";
              winhighlight = "Normal:Normal,FloatBorder:FloatBorder,CursorLine:Visual,Search:None";
            };
            documentation = {
              border = "rounded";
            };
          };

          formatting = {
            fields = [ "kind" "abbr" "menu" ];
            format = ''
              function(entry, item)
                local icons = {
                  Text = "",
                  Method = "󰆧",
                  Function = "󰊕",
                  Constructor = "",
                  Field = "󰇽",
                  Variable = "󰂡",
                  Class = "󰠱",
                  Interface = "",
                  Module = "",
                  Property = "󰜢",
                  Unit = "",
                  Value = "󰎠",
                  Enum = "",
                  Keyword = "󰌋",
                  Snippet = "",
                  Color = "󰏘",
                  File = "󰈙",
                  Reference = "",
                  Folder = "󰉋",
                  EnumMember = "",
                  Constant = "󰏿",
                  Struct = "",
                  Event = "",
                  Operator = "󰆕",
                  TypeParameter = "󰅲",
                }
                item.kind = string.format('%s %s', icons[item.kind], item.kind)
                item.menu = ({
                  nvim_lsp = "[LSP]",
                  luasnip = "[Snippet]",
                  buffer = "[Buffer]",
                  path = "[Path]",
                })[entry.source.name]
                return item
              end
            '';
          };
        };
      };

      # Snippet engine
      luasnip = {
        enable = true;
        settings = {
          enable_autosnippets = true;
          store_selection_keys = "<Tab>";
        };
        fromVscode = [
          {
            lazyLoad = true;
            paths = "${pkgs.vimPlugins.friendly-snippets}";
          }
        ];
      };

      # LSP signature help
      lsp-signature = {
        enable = true;
        settings = {
          bind = true;
          handler_opts = {
            border = "rounded";
          };
          hint_enable = true;
          hint_prefix = " ";
        };
      };

      # Trouble for better diagnostics list
      trouble = {
        enable = true;
        settings = {
          auto_open = false;
          auto_close = true;
          use_diagnostic_signs = true;
        };
      };

      # LSP progress indicator
      fidget = {
        enable = true;
        notification = {
          window = {
            winblend = 0;
          };
        };
      };
    };
  };
}
