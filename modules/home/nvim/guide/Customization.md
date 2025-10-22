# Customization Guide

This guide will help you customize the Nixvim configuration to suit your specific needs.

## 🎨 Changing the Theme

### Using Different Catppuccin Flavors

Edit `ui.nix`:

```nix
colorschemes.catppuccin = {
  enable = true;
  settings = {
    flavour = "mocha";      # Options: "mocha", "macchiato", "frappe", "latte"
    transparent_background = false;  # Set to true for transparent background
  };
};
```

### Switching to a Different Theme

Replace the catppuccin section in `ui.nix` with another colorscheme:

```nix
# Tokyo Night
colorschemes.tokyonight = {
  enable = true;
  settings = {
    style = "night";  # Options: "storm", "night", "moon", "day"
  };
};

# Or Gruvbox
colorschemes.gruvbox = {
  enable = true;
  settings = {
    contrast = "hard";  # Options: "soft", "medium", "hard"
  };
};

# Or Kanagawa
colorschemes.kanagawa = {
  enable = true;
  settings = {
    theme = "wave";  # Options: "wave", "dragon", "lotus"
  };
};
```

## 🔧 Adding New Language Support

### Example: Adding Rust Support

1. **Edit `lsp.nix`** - Add LSP server:

```nix
servers = {
  # ... existing servers ...

  rust-analyzer = {
    enable = true;
    installCargo = true;
    installRustc = true;
    settings = {
      cargo = {
        allFeatures = true;
        loadOutDirsFromCheck = true;
        runBuildScripts = true;
      };
      checkOnSave = {
        command = "clippy";
      };
      procMacro = {
        enable = true;
      };
    };
  };
};
```

2. **Add Treesitter parser** in `plugins.nix`:

```nix
treesitter = {
  settings = {
    ensure_installed = [
      # ... existing parsers ...
      "rust"
      "toml"  # For Cargo.toml
    ];
  };
};
```

3. **Add formatter** in `lsp.nix`:

```nix
conform-nvim = {
  settings = {
    formatters_by_ft = {
      # ... existing formatters ...
      rust = [ "rustfmt" ];
    };
  };
};
```

4. **Add packages** to `default.nix`:

```nix
extraPackages = with pkgs; [
  # ... existing packages ...
  rust-analyzer
  rustfmt
  clippy
];
```

### Example: Adding Go Support

```nix
# In lsp.nix
servers = {
  gopls = {
    enable = true;
  };
};

# In plugins.nix - treesitter
ensure_installed = [ "go" "gomod" "gosum" ];

# In lsp.nix - formatters
formatters_by_ft = {
  go = [ "gofmt" "goimports" ];
};

# In default.nix
extraPackages = with pkgs; [
  gopls
  gotools  # includes goimports
];
```

## ⌨️ Customizing Keybindings

### Adding New Keybindings

Edit `keybindings.nix`:

```nix
keymaps = [
  # ... existing keymaps ...

  # Custom save and quit
  {
    mode = "n";
    key = "<leader>wq";
    action = "<cmd>wq<CR>";
    options = {
      desc = "Save and quit";
      silent = true;
    };
  }

  # Quick terminal
  {
    mode = "n";
    key = "<C-t>";
    action = "<cmd>ToggleTerm<CR>";
    options = {
      desc = "Toggle terminal";
      silent = true;
    };
  }

  # Buffer close without closing window
  {
    mode = "n";
    key = "<leader>bx";
    action = "<cmd>Bdelete!<CR>";
    options = {
      desc = "Force close buffer";
      silent = true;
    };
  }
];
```

### Changing the Leader Key

Edit `core.nix`:

```nix
globals = {
  mapleader = ",";          # Change from space to comma
  maplocalleader = "\\";    # Local leader
};
```

## 🔌 Adding New Plugins

### Example: Adding Harpoon for Quick File Navigation

Edit `plugins.nix`:

```nix
plugins = {
  # ... existing plugins ...

  harpoon = {
    enable = true;
    enableTelescope = true;
    keymaps = {
      addFile = "<leader>ha";
      toggleQuickMenu = "<leader>hm";
      navFile = {
        "1" = "<leader>h1";
        "2" = "<leader>h2";
        "3" = "<leader>h3";
        "4" = "<leader>h4";
      };
      navNext = "<leader>hn";
      navPrev = "<leader>hp";
    };
  };
};
```

### Example: Adding Copilot

```nix
plugins = {
  copilot-lua = {
    enable = true;
    suggestion = {
      enabled = true;
      autoTrigger = true;
      keymap = {
        accept = "<M-l>";
        next = "<M-]>";
        prev = "<M-[>";
        dismiss = "<C-]>";
      };
    };
    panel = {
      enabled = true;
    };
  };
};
```

### Example: Adding Neorg for Note-Taking

```nix
plugins = {
  neorg = {
    enable = true;
    modules = {
      "core.defaults" = {
        __empty = null;
      };
      "core.concealer" = {
        __empty = null;
      };
      "core.dirman" = {
        config = {
          workspaces = {
            notes = "~/notes";
            work = "~/work/notes";
          };
          default_workspace = "notes";
        };
      };
    };
  };
};
```

## 📝 Adjusting Editor Options

### Change Indentation

Edit `core.nix`:

```nix
opts = {
  # For 2-space indentation
  tabstop = 2;
  shiftwidth = 2;
  softtabstop = 2;

  # Or for tabs instead of spaces
  expandtab = false;  # Use actual tab characters
};
```

### Enable Relative Line Numbers Only

```nix
opts = {
  number = false;        # Disable absolute line numbers
  relativenumber = true; # Keep relative
};
```

### Change Scroll Behavior

```nix
opts = {
  scrolloff = 15;     # More context lines
  sidescrolloff = 15; # More horizontal context
};
```

## 🎯 LSP Customization

### Change Diagnostic Display

Edit `lsp.nix`:

```nix
# Add after the lsp.enable = true;
lsp = {
  enable = true;

  # Custom diagnostic configuration
  onAttach = ''
    vim.diagnostic.config({
      virtual_text = {
        prefix = '●',
        severity = { min = vim.diagnostic.severity.WARN },
      },
      signs = true,
      underline = true,
      update_in_insert = false,
      severity_sort = true,
      float = {
        border = 'rounded',
        source = 'always',
        header = ',
        prefix = '',
      },
    })
  '';
};
```

### Add Custom LSP Handlers

```nix
extraConfigLua = ''
  -- Custom hover handler with borders
  vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(
    vim.lsp.handlers.hover, {
      border = "rounded",
      width = 60,
    }
  )

  -- Custom signature help
  vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(
    vim.lsp.handlers.signature_help, {
      border = "rounded",
      focusable = false,
      relative = "cursor",
    }
  )
'';
```

## 🔍 Telescope Customization

### Change Telescope Layout

Edit `plugins.nix`:

```nix
telescope = {
  settings = {
    defaults = {
      layout_strategy = "vertical";  # Change from horizontal
      layout_config = {
        vertical = {
          width = 0.9;
          height = 0.9;
          preview_height = 0.5;
        };
      };
    };
  };
};
```

### Add Custom Telescope Pickers

```nix
extraConfigLua = ''
  -- Custom picker for dotfiles
  vim.keymap.set('n', '<leader>fd', function()
    require('telescope.builtin').find_files({
      prompt_title = 'Dotfiles',
      cwd = '~/.config',
      hidden = true,
    })
  end, { desc = 'Find dotfiles' })
'';
```

## 🎨 Statusline Customization

### Simplify Lualine

Edit `ui.nix`:

```nix
lualine = {
  settings = {
    options = {
      theme = "catppuccin";
      section_separators = { left = ""; right = ""; };  # No separators
      component_separators = { left = ""; right = ""; };
    };
    sections = {
      lualine_a = [ "mode" ];
      lualine_b = [ "branch" ];
      lualine_c = [ "filename" ];
      lualine_x = [ "filetype" ];
      lualine_y = [ ];
      lualine_z = [ "location" ];
    };
  };
};
```

### Add Custom Components

```nix
lualine = {
  settings = {
    sections = {
      lualine_x = [
        # Show current function/class
        {
          __unkeyed-1.__raw = ''
            function()
              local navic = require("nvim-navic")
              if navic.is_available() then
                return navic.get_location()
              end
              return ""
            end
          '';
        }
        "filetype"
      ];
    };
  };
};
```

## 📦 Module Organization

### Creating Custom Modules

Create a new file `custom.nix`:

```nix
# custom.nix - Your personal customizations
{ config, pkgs, lib, ... }:

{
  programs.nixvim = {
    # Your custom plugins
    plugins = {
      # Example: enable additional plugins
    };

    # Your custom keymaps
    keymaps = [
      # Your mappings
    ];

    # Custom Lua configuration
    extraConfigLua = ''
      -- Your custom Lua code
    '';
  };
}
```

Then import it in `default.nix`:

```nix
imports = [
  ./core.nix
  ./ui.nix
  ./lsp.nix
  ./plugins.nix
  ./keybindings.nix
  ./custom.nix  # Your customizations
];
```

## 🚀 Performance Optimization

### Lazy Loading Specific Plugins

```nix
plugins = {
  # Load only when needed
  neorg = {
    enable = true;
    lazy = true;
    cmd = [ "Neorg" ];
    ft = [ "norg" ];
  };

  markdown-preview = {
    enable = true;
    lazy = true;
    ft = [ "markdown" ];
  };
};
```

### Disable Unused Features

In `core.nix`:

```nix
globals = {
  # Disable if you don't use these
  loaded_perl_provider = 0;
  loaded_ruby_provider = 0;
  loaded_node_provider = 0;
};
```

## 💡 Tips

1. **Test Changes Incrementally**: Make small changes and rebuild to catch errors early
2. **Use `extraConfigLua`**: For complex Lua configurations that don't have Nix options yet
3. **Check Plugin Documentation**: Visit the plugin's GitHub page for all available options
4. **Backup**: Keep your working configuration before making major changes
5. **Use Which-Key**: Press `<Space>` and wait to discover keybindings

## 🔗 Useful Resources

- [Nixvim Options Search](https://nix-community.github.io/nixvim/)
- [Neovim Plugin Directory](https://dotfyle.com/neovim/plugins/)
- [Awesome Neovim](https://github.com/rockerBOO/awesome-neovim)
