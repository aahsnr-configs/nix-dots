# Quick Start Guide

Get up and running with your new Nixvim configuration in minutes!

## 📦 Installation

### Step 1: Create Directory Structure

```bash
mkdir -p ~/.config/nixvim
cd ~/.config/nixvim
```

### Step 2: Download Configuration Files

Create these files in `~/.config/nixvim/`:

1. `default.nix` - Main entry point
2. `core.nix` - Core settings
3. `ui.nix` - UI and theming
4. `lsp.nix` - LSP configuration
5. `plugins.nix` - Additional plugins
6. `keybindings.nix` - Keybindings

All files are provided in the artifacts above.

### Step 3: Integrate with Home Manager

Add to your `~/.config/home-manager/home.nix`:

```nix
{ config, pkgs, ... }:

{
  imports = [
    ./nixvim  # Points to ~/.config/nixvim/default.nix
  ];

  # Install a Nerd Font (REQUIRED for icons)
  home.packages = with pkgs; [
    (nerdfonts.override { fonts = [ "JetBrainsMono" ]; })
  ];

  fonts.fontconfig.enable = true;

  # Set Neovim as default editor
  home.sessionVariables = {
    EDITOR = "nvim";
    VISUAL = "nvim";
  };

  # Rest of your configuration...
}
```

### Step 4: Apply Configuration

```bash
home-manager switch
```

### Step 5: Configure Your Terminal

**Set your terminal to use the Nerd Font:**

#### Alacritty (`~/.config/alacritty/alacritty.yml`):

```yaml
font:
  normal:
    family: "JetBrainsMono Nerd Font"
  size: 12.0
```

#### Kitty (`~/.config/kitty/kitty.conf`):

```
font_family JetBrainsMono Nerd Font
font_size 12.0
```

#### WezTerm (`~/.config/wezterm/wezterm.lua`):

```lua
local wezterm = require 'wezterm'
local config = {}

config.font = wezterm.font("JetBrainsMono Nerd Font")
config.font_size = 12.0

return config
```

## 🚀 First Launch

### Start Neovim

```bash
nvim
```

You should see the beautiful dashboard!

### Wait for Initial Setup

On first launch:

- Treesitter will compile parsers (1-2 minutes)
- LSP servers will initialize
- Plugins will lazy-load as needed

## ✅ Verify Everything Works

### Test 1: Dashboard

- Start `nvim` - you should see the ASCII art dashboard
- Press `q` to close

### Test 2: File Navigation

```bash
nvim .
```

- Press `<leader>ff` (Space + f + f) for file finder
- Type to search, Enter to open

### Test 3: LSP (Python)

```bash
nvim test.py
```

Type:

```python
def hello(name):
    print(f"Hello, {name}")
```

- Check for syntax highlighting (should be colorful)
- Type `hell` and press `<C-Space>` - completion should appear
- Hover over `print` and press `K` - documentation should appear
- Save with `<C-s>` - should auto-format

### Test 4: Git Integration

In a git repository:

```bash
nvim README.md
```

- Make changes
- Look for git indicators in the sign column
- Press `<leader>gg` for LazyGit

### Test 5: Telescope

```bash
nvim
```

- `<leader>ff` - Find files
- `<leader>fg` - Search text in files
- `<leader>fb` - List open buffers

## 🎯 Essential Keybindings

### File Operations

| Key          | Action                 |
| ------------ | ---------------------- |
| `<leader>ff` | Find files             |
| `<leader>fg` | Search in files (grep) |
| `<leader>fb` | Find buffers           |
| `<leader>e`  | Toggle file tree       |
| `<C-s>`      | Save file              |

### Window Navigation

| Key           | Action               |
| ------------- | -------------------- |
| `<C-h/j/k/l>` | Move between windows |
| `<leader>sv`  | Split vertically     |
| `<leader>sh`  | Split horizontally   |

### LSP Actions

| Key          | Action             |
| ------------ | ------------------ |
| `gd`         | Go to definition   |
| `gr`         | Find references    |
| `K`          | Show documentation |
| `<leader>la` | Code action        |
| `<leader>lr` | Rename             |

### Git

| Key          | Action              |
| ------------ | ------------------- |
| `<leader>gg` | Open LazyGit        |
| `]c`         | Next git change     |
| `[c`         | Previous git change |

### Terminal

| Key          | Action                        |
| ------------ | ----------------------------- |
| `<C-\>`      | Toggle terminal               |
| `<leader>tt` | Toggle terminal (alternative) |

### Code Editing

| Key          | Action              |
| ------------ | ------------------- |
| `gcc`        | Toggle line comment |
| `<leader>cf` | Format buffer       |
| `<C-Space>`  | Trigger completion  |

**Pro Tip**: Press `<Space>` (leader) and wait - Which-Key will show you all available keybindings!

## 🎨 Customize Your Setup

### Change Color Scheme

Edit `ui.nix`:

```nix
colorschemes.catppuccin = {
  settings = {
    flavour = "macchiato";  # Try: mocha, macchiato, frappe, latte
  };
};
```

### Add Python Project Support

For a Python project:

1. Create a virtual environment:

```bash
python -m venv .venv
source .venv/bin/activate
pip install your-packages
```

2. In Neovim:

```vim
:VenvSelect
" Or press <leader>cv
```

3. Select your `.venv` from the list

Now LSP will recognize your installed packages!

### Adjust Indentation

Edit `core.nix`:

```nix
opts = {
  tabstop = 2;        # Change from 4 to 2
  shiftwidth = 2;
  softtabstop = 2;
};
```

## 🐛 Quick Troubleshooting

### Icons Not Showing?

1. Verify Nerd Font is installed: `fc-list | grep "JetBrains"`
2. Check terminal is using the font
3. Restart terminal after font change

### LSP Not Working?

```vim
:LspInfo          " Check if LSP attached
:checkhealth lsp  " Run health check
```

### Slow Startup?

```vim
:Lazy profile     " See what's taking time
```

### Something Broken?

```bash
# Reset Neovim data
rm -rf ~/.local/share/nvim
rm -rf ~/.cache/nvim

# Rebuild
home-manager switch
```

## 📚 Next Steps

### Learn the Workflow

1. **Day 1-3**: Get comfortable with basic navigation and file operations
2. **Week 1**: Master LSP features (go to definition, find references, rename)
3. **Week 2**: Learn Telescope thoroughly - it's your command center
4. **Week 3**: Customize keybindings to match your workflow
5. **Month 1**: Add language support for your other projects

### Explore Features

Try these commands to discover more:

```vim
:Telescope         " Browse all Telescope features
:Mason            " Manage LSP servers
:Lazy             " Manage plugins
:Trouble          " Better diagnostics view
:TodoTelescope    " Find all TODOs in project
```

### Join the Community

- **NixOS Discourse**: Great for Nix-specific questions
- **r/neovim**: General Neovim help and inspiration
- **GitHub Issues**: Report bugs or request features

## 🎓 Learning Resources

### For Beginners

1. **Neovim Basics**: Type `vimtutor` in terminal for interactive tutorial
2. **Modal Editing**: Learn the vim way - it's a game changer!
3. **Telescope**: Your most important tool - master `<leader>ff` and `<leader>fg`

### For Advanced Users

1. **Custom Plugins**: Add your own in `plugins.nix`
2. **Lua Config**: Use `extraConfigLua` for advanced customization
3. **Custom LSP**: Configure language-specific settings in `lsp.nix`

## 💡 Pro Tips

### Productivity Boosters

1. **Use Telescope for everything**: Files, buffers, git files, grep, symbols
2. **Learn LSP shortcuts**: `gd`, `gr`, `K`, `<leader>lr` will save hours
3. **Master visual mode**: `V` for line, `v` for character, `<C-v>` for block
4. **Use marks**: `ma` to set mark a, `'a` to jump back
5. **Buffer management**: `<S-h>` and `<S-l>` to switch, `<leader>bd` to close

### Time Savers

```vim
" Open file under cursor
gf

" Go back to previous location
<C-o>

" Go forward
<C-i>

" Search and replace in file
:%s/old/new/gc

" Format entire file
gg=G

" Save and quit
<leader>wq
```

## 🎉 You're Ready!

Your Neovim is now a powerful IDE with:

- ✅ Beautiful UI with Catppuccin theme
- ✅ Intelligent code completion
- ✅ Powerful file navigation
- ✅ Git integration
- ✅ LSP for Python, Nix, and Markdown
- ✅ Auto-formatting on save
- ✅ And much more!

**Enjoy your new development environment!** 🚀

---

Need help? Check the `Troubleshooting Guide` or `Customization Guide` for more details!
