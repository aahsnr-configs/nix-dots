# Comprehensive Nixvim Configuration

A modern, feature-rich Neovim configuration built with Nixvim, designed for productive development with Python, Nix, and Markdown.

## 📁 Structure

```
nixvim/
├── default.nix      # Main entry point
├── core.nix         # Core Neovim settings
├── ui.nix           # UI plugins and theming
├── lsp.nix          # LSP and completion configuration
├── plugins.nix      # Additional plugins
└── keybindings.nix  # Custom keybindings
```

## 🚀 Installation

### With Home Manager

Add to your `home.nix` or `home-manager` configuration:

```nix
{ config, pkgs, ... }:

{
  imports = [
    ./path/to/nixvim/default.nix
  ];
}
```

Then rebuild your home-manager configuration:

```bash
home-manager switch
```

### Standalone

You can also use this as a standalone flake:

```nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixvim.url = "github:nix-community/nixvim";
  };

  outputs = { self, nixpkgs, nixvim }: {
    # Your configuration here
  };
}
```

## ✨ Features

### Language Support

#### Python

- **LSP**: Pyright for type checking and intellisense
- **Linting**: Ruff for fast linting
- **Formatting**: Black for code formatting
- **Virtual Environments**: Built-in venv selector (`<leader>cv`)

#### Nix

- **LSP**: nil for Nix language support
- **Formatting**: nixpkgs-fmt for consistent formatting

#### Markdown

- **Enhanced Syntax**: Treesitter-based highlighting
- **Live Preview**: Real-time markdown preview (`<leader>mp`)
- **Linting**: markdownlint integration

### UI & Theming

- **Theme**: Catppuccin Mocha with excellent plugin integration
- **Dashboard**: Beautiful startup screen with snacks.nvim
- **Statusline**: Informative lualine with file info, Git status, LSP diagnostics, and clock
- **File Icons**: Nerd Font icons throughout the UI
- **Notifications**: Modern notification system via snacks

### Core Features

#### File Navigation

- **Telescope**: Fuzzy finder for files, buffers, and text
  - `<leader>ff` - Find files
  - `<leader>fg` - Live grep
  - `<leader>fb` - Find buffers
  - `<leader>fh` - Help tags
  - `<leader>fr` - Recent files

#### Code Intelligence

- **Autocompletion**: nvim-cmp with LSP, snippet, and buffer sources
- **Snippets**: LuaSnip with friendly-snippets
- **Auto-formatting**: Conform.nvim for format-on-save
- **Treesitter**: Advanced syntax highlighting and text objects
- **Inlay Hints**: Type and parameter hints
- **CodeLens**: Contextual information display
- **Semantic Tokens**: Enhanced LSP-based highlighting

#### Git Integration

- **Gitsigns**: Git changes in sign column with inline blame
- **LazyGit**: Full-featured Git UI (`<leader>gg`)
- Git navigation: `]c` (next hunk), `[c` (previous hunk)
- Hunk operations: `<leader>gs` (stage), `<leader>gr` (reset)

#### Additional Tools

- **Autopairs**: Automatic bracket/quote pairing
- **Comment**: Easy code commenting (`gcc`, `gbc`)
- **Surround**: Manipulate surrounding characters
- **Todo Comments**: Highlight and search TODO, FIXME, etc.
- **Trouble**: Better diagnostics list (`<leader>xx`)
- **Neo-tree**: File explorer (`<leader>e`)
- **Terminal**: Integrated terminal (`<C-\>` or `<leader>tt`)

## ⌨️ Key Bindings

### Leader Key

The leader key is set to `<Space>`.

### General

| Key         | Mode                 | Action                  |
| ----------- | -------------------- | ----------------------- |
| `<Esc>`     | Normal               | Clear search highlights |
| `<C-s>`     | Normal/Insert/Visual | Save file               |
| `<leader>q` | Normal               | Quit all                |

### Window Navigation

| Key                      | Action                   |
| ------------------------ | ------------------------ |
| `<C-h/j/k/l>`            | Navigate between windows |
| `<C-Up/Down/Left/Right>` | Resize windows           |
| `<leader>sv`             | Vertical split           |
| `<leader>sh`             | Horizontal split         |
| `<leader>sc`             | Close split              |

### Buffer Management

| Key          | Action                            |
| ------------ | --------------------------------- |
| `<S-h>`      | Previous buffer                   |
| `<S-l>`      | Next buffer                       |
| `<leader>bd` | Delete buffer                     |
| `<leader>bD` | Delete all buffers except current |

### File Finding (Telescope)

| Key          | Action                   |
| ------------ | ------------------------ |
| `<leader>ff` | Find files               |
| `<leader>fg` | Live grep                |
| `<leader>fb` | Find buffers             |
| `<leader>fh` | Help tags                |
| `<leader>fr` | Recent files             |
| `<leader>fc` | Find string under cursor |
| `<leader>ft` | Find TODOs               |
| `<leader>fs` | Document symbols         |
| `<leader>fS` | Workspace symbols        |
| `<leader>fd` | Workspace diagnostics    |

### LSP Actions

| Key          | Action                   |
| ------------ | ------------------------ |
| `gd`         | Go to definition         |
| `gD`         | Go to declaration        |
| `K`          | Hover documentation      |
| `gi`         | Go to implementation     |
| `gr`         | Find references          |
| `<leader>lr` | Rename symbol            |
| `<leader>la` | Code action              |
| `<leader>lf` | Format buffer            |
| `<leader>li` | LSP info                 |
| `[d` / `]d`  | Previous/next diagnostic |

### Git

| Key          | Action             |
| ------------ | ------------------ |
| `<leader>gg` | Open LazyGit       |
| `]c` / `[c`  | Next/previous hunk |
| `<leader>gs` | Stage hunk         |
| `<leader>gr` | Reset hunk         |
| `<leader>gp` | Preview hunk       |
| `<leader>gb` | Blame line         |

### Code Editing

| Key       | Mode   | Action                |
| --------- | ------ | --------------------- |
| `gcc`     | Normal | Toggle line comment   |
| `gbc`     | Normal | Toggle block comment  |
| `<` / `>` | Visual | Indent left/right     |
| `J` / `K` | Visual | Move text down/up     |
| `p`       | Visual | Paste without yanking |

### Other

| Key          | Action                     |
| ------------ | -------------------------- |
| `<leader>e`  | Toggle file explorer       |
| `<leader>xx` | Toggle Trouble diagnostics |
| `<leader>mp` | Markdown preview           |
| `<leader>cv` | Select Python venv         |
| `<leader>tt` | Toggle terminal            |
| `<C-\>`      | Toggle terminal            |

### Discovering More Keybindings

Press `<Space>` (leader key) and wait - Which-Key will show you all available keybindings!

## 🎨 Customization

### Adding New Languages

To add support for a new language, edit `lsp.nix`:

```nix
# Add LSP server
servers = {
  # ... existing servers ...

  rust-analyzer = {
    enable = true;
    # ... configuration ...
  };
};

# Add to treesitter
ensure_installed = [
  # ... existing parsers ...
  "rust"
];

# Add formatter to conform-nvim
formatters_by_ft = {
  # ... existing formatters ...
  rust = [ "rustfmt" ];
};
```

Don't forget to add the necessary packages to `extraPackages` in `default.nix`.

### Changing Theme

To change from Catppuccin Mocha to another theme, edit `ui.nix`:

```nix
colorschemes.catppuccin = {
  enable = true;
  settings = {
    flavour = "macchiato"; # or "frappe", "latte"
  };
};
```

Or use a different colorscheme entirely:

```nix
colorschemes.tokyonight = {
  enable = true;
  settings = {
    style = "night";
  };
};
```

### Adding Plugins

To add new plugins, edit `plugins.nix`:

```nix
plugins = {
  # ... existing plugins ...

  harpoon = {
    enable = true;
    # ... configuration ...
  };
};
```

### Modifying Keybindings

Edit `keybindings.nix` to add or modify keybindings:

```nix
keymaps = [
  # ... existing keymaps ...

  {
    mode = "n";
    key = "<leader>my";
    action = "<cmd>MyCommand<CR>";
    options = {
      desc = "My custom command";
      silent = true;
    };
  }
];
```

## 🔧 Troubleshooting

### LSP Not Working

1. Check if the language server is installed:

   ```bash
   nix-store -q --references ~/.nix-profile | grep <lsp-name>
   ```

2. View LSP logs:
   ```vim
   :LspInfo
   :LspLog
   ```

### Treesitter Parser Issues

Force reinstall parsers:

```vim
:TSInstall <language>
```

### Format on Save Not Working

Check conform-nvim status:

```vim
:ConformInfo
```

## 📚 Resources

- [Nixvim Documentation](https://nix-community.github.io/nixvim/)
- [Neovim Documentation](https://neovim.io/doc/)
- [Home Manager Manual](https://nix-community.github.io/home-manager/)

## 🤝 Contributing

This configuration is modular by design. Feel free to:

1. Fork and modify for your needs
2. Submit improvements via pull requests
3. Share your custom modules

## 📄 License

This configuration is provided as-is for educational and productivity purposes.

---

**Happy Coding! 🚀**
