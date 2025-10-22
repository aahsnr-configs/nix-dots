# Troubleshooting Guide

Common issues and their solutions for the Nixvim configuration.

## 🔄 Reset and Clean Start

### Complete Reset

If things are completely broken:

```bash
# Remove Neovim data
rm -rf ~/.local/share/nvim
rm -rf ~/.local/state/nvim
rm -rf ~/.cache/nvim

# Rebuild home-manager
home-manager switch
```

### Minimal Test Configuration

Create a minimal test to isolate issues:

```nix
# minimal-test.nix
{ pkgs, ... }:
{
  programs.nixvim = {
    enable = true;

    colorschemes.catppuccin.enable = true;

    plugins = {
      telescope.enable = true;
      treesitter.enable = true;
    };
  };
}
```

Then import only this file temporarily.

## 📞 Community Resources

### Where to Ask for Help

1. **Nixvim Discussions**: [GitHub Discussions](https://github.com/nix-community/nixvim/discussions)
2. **NixOS Discourse**: [discourse.nixos.org](https://discourse.nixos.org/)
3. **Neovim Subreddit**: [r/neovim](https://www.reddit.com/r/neovim/)
4. **NixOS Matrix**: #nixos:matrix.org

### Reporting Bugs

When reporting issues, include:

1. **Your system info**:

   ```bash
   nix-shell -p nix-info --run "nix-info -m"
   ```

2. **Nixvim version**:

   ```bash
   nix flake metadata github:nix-community/nixvim
   ```

3. **Minimal reproducible configuration**
4. **Error messages** (full output)
5. **Steps to reproduce**

## 💡 Pro Tips

### Quick Fixes

1. **Rebuild with fresh cache**:

   ```bash
   home-manager switch --option eval-cache false
   ```

2. **Force rebuild everything**:

   ```bash
   home-manager switch --no-out-link
   ```

3. **Check for updates**:
   ```bash
   nix flake update
   ```

### Prevention

1. **Version lock critical dependencies** in your flake.nix
2. **Commit working configurations** to git
3. **Test changes in a separate branch**
4. **Keep a backup of your last working configuration**

## 🎯 Common Error Messages

### "attribute 'X' missing"

**Error**:

```
error: attribute 'somePlugin' missing
```

**Solution**: The plugin name in nixvim might be different from the plugin name. Check the [nixvim options](https://nix-community.github.io/nixvim/).

### "infinite recursion encountered"

**Error**:

```
error: infinite recursion encountered
```

**Solution**: You have a circular dependency. Check your imports and make sure modules don't import each other circularly.

### "cannot coerce a function to a string"

**Error**:

```
error: cannot coerce a function to a string
```

**Solution**: You're likely missing `__raw` around a Lua function:

```nix
# Wrong
callback = "function() ... end";

# Correct
callback.__raw = "function() ... end";
```

### "builder for 'X' failed with exit code 1"

**Error**:

```
builder for 'home-manager-generation' failed with exit code 1
```

**Solution**:

1. Run with `--show-trace` to see the full error
2. Check your Nix syntax
3. Verify all plugin names are correct

### "The option 'programs.nixvim.plugins.X' does not exist"

**Error**:

```
The option 'programs.nixvim.plugins.somePlugin' does not exist
```

**Solution**:

1. Plugin might not be available in nixvim yet
2. Check the correct option path in [nixvim docs](https://nix-community.github.io/nixvim/)
3. Use `extraPlugins` for plugins not in nixvim:

```nix
extraPlugins = with pkgs.vimPlugins; [
  some-custom-plugin
];
```

## 🧪 Testing Checklist

Before considering your configuration "done", test:

- [ ] Start Neovim (no errors on startup)
- [ ] Open a Python file (LSP attaches)
- [ ] Trigger completion (`<C-Space>`)
- [ ] Format a file (`:w` should auto-format)
- [ ] Search files (`<leader>ff`)
- [ ] Grep text (`<leader>fg`)
- [ ] Open file tree (`<leader>e`)
- [ ] Toggle terminal (`<C-\>`)
- [ ] Git operations (`<leader>gg`)
- [ ] Go to definition (`gd`)
- [ ] Find references (`gr`)
- [ ] Rename symbol (`<leader>lr`)
- [ ] View diagnostics (`<leader>xx`)
- [ ] Select Python venv (`<leader>cv`)
- [ ] Preview markdown (`<leader>mp`)

## 🔍 Advanced Debugging

### Enable Debug Logging

Add to your configuration:

```nix
extraConfigLua = ''
  -- Enable debug logging
  vim.lsp.set_log_level("debug")

  -- Log all autocmds
  vim.api.nvim_create_autocmd("*", {
    callback = function(ev)
      print(string.format('Event: %s, Buffer: %s', ev.event, ev.buf))
    end,
  })
'';
```

### Inspect Lua State

In Neovim:

```vim
" Check if a module is loaded
:lua print(package.loaded['telescope'])

" Inspect plugin config
:lua print(vim.inspect(require('telescope.config').values))

" Check keymaps
:lua print(vim.inspect(vim.api.nvim_get_keymap('n')))

" View all autocommands
:autocmd

" Check buffer options
:lua print(vim.inspect(vim.bo))
```

### Profile Plugin Loading

```vim
:Lazy profile
```

Look for:

- Plugins taking >50ms to load
- Plugins loaded at startup that could be lazy-loaded
- Duplicate plugin loads

### Network Issues

If plugins fail to download:

```bash
# Test network connectivity
curl -I https://github.com

# Use a mirror
nix-channel --add https://mirrors.tuna.tsinghua.edu.cn/nix-channels/nixpkgs-unstable nixpkgs
```

## 📚 Reference Configuration

### Working Minimal Setup

If you're having issues, start with this minimal working config:

```nix
{ pkgs, ... }:
{
  programs.nixvim = {
    enable = true;

    colorschemes.catppuccin = {
      enable = true;
      settings.flavour = "mocha";
    };

    opts = {
      number = true;
      relativenumber = true;
      shiftwidth = 2;
    };

    plugins = {
      lualine.enable = true;
      telescope.enable = true;
      treesitter.enable = true;

      lsp = {
        enable = true;
        servers.nil-ls.enable = true;
      };
    };

    extraPackages = with pkgs; [ nil ];
  };
}
```

Test this first, then gradually add more features.

## 🎓 Learning Resources

### Understanding Nixvim

1. **Read the source**: Browse the [nixvim GitHub repo](https://github.com/nix-community/nixvim)
2. **Check examples**: Look at [community configurations](https://github.com/topics/nixvim)
3. **Understand Nix**: Learn Nix language basics at [nix.dev](https://nix.dev)

### Neovim Concepts

1. **LSP**: [Neovim LSP docs](https://neovim.io/doc/user/lsp.html)
2. **Treesitter**: [nvim-treesitter docs](https://github.com/nvim-treesitter/nvim-treesitter)
3. **Telescope**: [telescope.nvim docs](https://github.com/nvim-telescope/telescope.nvim)

## 🔧 Emergency Commands

### In Neovim

```vim
" Start with no plugins
nvim --noplugin

" Start with no config
nvim -u NONE

" Start with minimal config
nvim -u NORC

" Disable all autocommands
:noautocmd edit file.txt
```

### In Terminal

```bash
# Rollback home-manager
home-manager generations
/nix/store/xxx-home-manager-generation/activate

# List all available generations
home-manager generations

# Remove old generations
nix-collect-garbage -d
```

## 🎯 Still Stuck?

If you've tried everything:

1. **Create a GitHub Gist** with your configuration
2. **Post on NixOS Discourse** with:
   - Your configuration (minimal reproducible example)
   - Error messages
   - System information
   - What you've tried

3. **Join the Matrix chat** for real-time help
4. **Check existing issues** on the nixvim repository

Remember: The community is friendly and helpful. Don't hesitate to ask questions!

---

**Last Updated**: October 2025
**Nixvim Version**: Latest stable
🔍 General Debugging

### Check Neovim Health

Inside Neovim, run:

```vim
:checkhealth
```

This will show you any issues with providers, plugins, or dependencies.

### View Build Errors

When home-manager fails to build:

```bash
home-manager switch --show-trace
```

This shows detailed error messages and stack traces.

### Test Configuration Without Installing

```bash
nix build .#homeConfigurations.your-username.activationPackage --show-trace
```

## 🚫 Common Issues

### Issue: Icons Not Displaying

**Symptoms**: You see squares, question marks, or garbled characters instead of icons.

**Solution**:

1. Install a Nerd Font:

   ```nix
   # In home.nix
   home.packages = with pkgs; [
     (nerdfonts.override { fonts = [ "JetBrainsMono" ]; })
   ];

   fonts.fontconfig.enable = true;
   ```

2. Configure your terminal to use the Nerd Font:
   - **Alacritty**: Add to `~/.config/alacritty/alacritty.yml`:
     ```yaml
     font:
       normal:
         family: "JetBrainsMono Nerd Font"
     ```
   - **Kitty**: Add to `~/.config/kitty/kitty.conf`:
     ```
     font_family JetBrainsMono Nerd Font
     ```
   - **WezTerm**: Add to `~/.config/wezterm/wezterm.lua`:
     ```lua
     config.font = wezterm.font("JetBrainsMono Nerd Font")
     ```

### Issue: LSP Not Working

**Symptoms**: No autocompletion, no diagnostics, no go-to-definition.

**Diagnosis**:

```vim
:LspInfo
```

**Solutions**:

1. **LSP Server Not Found**:

   ```nix
   # Ensure the package is in extraPackages in default.nix
   extraPackages = with pkgs; [
     pyright  # Make sure it's here
   ];
   ```

2. **LSP Not Attaching**:

   ```vim
   # Check if LSP attached to buffer
   :lua print(vim.inspect(vim.lsp.get_active_clients()))
   ```

3. **Python Path Issues**:
   ```nix
   # In lsp.nix, configure pyright with specific Python path
   servers.pyright = {
     settings = {
       python = {
         pythonPath = "${pkgs.python3}/bin/python";
       };
     };
   };
   ```

### Issue: Treesitter Errors

**Symptoms**: "Parser not found" or "query error" messages.

**Solutions**:

1. **Missing Parsers**:

   ```nix
   # In plugins.nix, ensure parser is installed
   treesitter = {
     settings = {
       ensure_installed = [
         "python"
         "nix"
         "markdown"
         # Add your language here
       ];
     };
   };
   ```

2. **Parser Compile Errors**:
   ```bash
   # Clear Treesitter cache
   rm -rf ~/.local/share/nvim/site/pack/*/start/nvim-treesitter
   ```

### Issue: Telescope Not Finding Files

**Symptoms**: Telescope opens but shows no results.

**Solutions**:

1. **Missing Dependencies**:

   ```nix
   # In default.nix
   extraPackages = with pkgs; [
     ripgrep  # Required for live_grep
     fd       # Required for find_files
   ];
   ```

2. **Hidden Files Not Showing**:

   ```vim
   # Use this keybinding to toggle hidden files
   <C-h>  " in Telescope prompt
   ```

3. **Git Ignore Issues**:
   ```nix
   # In plugins.nix, modify telescope settings
   telescope = {
     settings = {
       defaults = {
         file_ignore_patterns = [ ];  # Clear default ignores
       };
       pickers = {
         find_files = {
           hidden = true;  # Show hidden files
           follow = true;  # Follow symlinks
         };
       };
     };
   };
   ```

### Issue: Format on Save Not Working

**Symptoms**: Code doesn't format when saving files.

**Diagnosis**:

```vim
:ConformInfo
```

**Solutions**:

1. **Formatter Not Installed**:

   ```nix
   # In default.nix
   extraPackages = with pkgs; [
     black  # For Python
     nixpkgs-fmt  # For Nix
   ];
   ```

2. **Formatter Not Configured**:

   ```nix
   # In lsp.nix
   conform-nvim = {
     settings = {
       formatters_by_ft = {
         python = [ "black" ];  # Ensure this matches your filetype
       };
     };
   };
   ```

3. **Conflicting Formatters**:
   ```vim
   # Disable LSP formatting if using conform
   :lua vim.lsp.buf.format = function() end
   ```

### Issue: Completion Not Working

**Symptoms**: No completion menu appears when typing.

**Solutions**:

1. **Check CMP Sources**:

   ```vim
   :lua print(vim.inspect(require('cmp').get_config()))
   ```

2. **Ensure LSP Is Running**:

   ```vim
   :LspInfo  # Should show attached servers
   ```

3. **Check Keybindings**:
   - Default: `<Tab>` to select next item
   - `<C-Space>` to manually trigger completion
   - `<CR>` to confirm

4. **Verify LuaSnip**:
   ```nix
   # In lsp.nix, ensure luasnip is properly configured
   plugins.luasnip = {
     enable = true;
     fromVscode = [
       {
         lazyLoad = true;
         paths = "${pkgs.vimPlugins.friendly-snippets}";
       }
     ];
   };
   ```

### Issue: Terminal Colors Look Wrong

**Symptoms**: Colors appear washed out or incorrect.

**Solutions**:

1. **Enable True Color Support**:
   - **Alacritty**: Already enabled by default
   - **Tmux**: Add to `~/.tmux.conf`:
     ```
     set -g default-terminal "screen-256color"
     set -ga terminal-overrides ",*256col*:Tc"
     ```

2. **Check COLORTERM**:

   ```bash
   echo $COLORTERM  # Should output "truecolor"
   ```

3. **Set in Shell Config**:
   ```bash
   export COLORTERM=truecolor
   ```

### Issue: Markdown Preview Not Opening

**Symptoms**: `:MarkdownPreview` command does nothing.

**Solutions**:

1. **Check Node.js**:

   ```bash
   node --version  # Should be installed
   ```

2. **Install Browser**:

   ```nix
   # In home.nix
   home.packages = with pkgs; [
     firefox  # Or your preferred browser
   ];
   ```

3. **Manual Port**:
   ```nix
   # In plugins.nix
   markdown-preview = {
     settings = {
       port = "8080";  # Try a different port
       browser = "firefox";  # Specify browser
     };
   };
   ```

### Issue: Python Virtual Environment Not Detected

**Symptoms**: LSP doesn't recognize packages installed in venv.

**Solutions**:

1. **Use VenvSelector**:

   ```vim
   :VenvSelect  # Or <leader>cv
   ```

2. **Manually Set Python Path**:

   ```vim
   :lua vim.g.python3_host_prog = '/path/to/venv/bin/python'
   ```

3. **Auto-detect in Project**:
   ```nix
   # In lsp.nix
   servers.pyright = {
     settings = {
       python = {
         analysis = {
           autoSearchPaths = true;
           useLibraryCodeForTypes = true;
         };
       };
     };
   };
   ```

### Issue: Slow Startup Time

**Symptoms**: Neovim takes several seconds to start.

**Diagnosis**:

```vim
:Lazy profile  # Shows plugin load times
```

**Solutions**:

1. **Enable Lazy Loading**:

   ```nix
   # In plugins.nix, add lazy loading to heavy plugins
   plugins = {
     telescope = {
       enable = true;
       lazy = true;
       cmd = [ "Telescope" ];
       keys = [
         { key = "<leader>ff"; action = "<cmd>Telescope find_files<cr>"; }
       ];
     };
   };
   ```

2. **Disable Unused Providers**:

   ```nix
   # In core.nix
   globals = {
     loaded_perl_provider = 0;
     loaded_ruby_provider = 0;
   };
   ```

3. **Reduce Treesitter Parsers**:
   ```nix
   # Only install parsers you actually use
   ensure_installed = [ "python" "nix" "markdown" ];  # Not "all"
   ```

### Issue: Git Signs Not Showing

**Symptoms**: No Git indicators in the sign column.

**Solutions**:

1. **Ensure In Git Repository**:

   ```bash
   git status  # Should not error
   ```

2. **Check Sign Column**:

   ```nix
   # In core.nix
   opts = {
     signcolumn = "yes";  # Always show sign column
   };
   ```

3. **Verify Gitsigns Config**:
   ```vim
   :Gitsigns toggle_signs  # Toggle on
   ```

## 🔧 Configuration Validation

### Test Individual Modules

You can test if a specific module builds correctly:

```bash
# Test just the LSP configuration
nix-instantiate --eval -E '(import <nixpkgs> {}).lib.evalModules { modules = [ ./lsp.nix ]; }'
```

### Syntax Check

Before building, check Nix syntax:

```bash
nix-instantiate --parse default.nix
```

### Dry Run

Test what would change without actually applying:

```bash
home-manager build
# Then check ./result
ls -la result/
```

## 📊 Performance Profiling

### Startup Time

```vim
:Lazy profile
```

Or use this command-line approach:

```bash
nvim --startuptime startup.log
cat startup.log | sort -k2 -n
```

### LSP Performance

```vim
:lua vim.lsp.set_log_level("debug")
:lua print(vim.lsp.get_log_path())
```

Then check the log file for issues.

## 🆘 Getting Help

### Check Logs

1. **Neovim Messages**:

   ```vim
   :messages
   ```

2. **LSP Log**:

   ```vim
   :LspLog
   ```

3. **Home Manager Log**:
   ```bash
   journalctl --user -u home-manager-$USER.service
   ```

### Debug Mode

Start Neovim with verbose output:

```bash
nvim -V9nvim.log
```

### Bisect Configuration

Comment out half of your plugins to find the problematic one:

```nix
# In plugins.nix
plugins = {
  # telescope.enable = true;  # Disable temporarily
  # treesitter.enable = true;
  # ... etc
};
```

##
