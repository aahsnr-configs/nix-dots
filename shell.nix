{ pkgs ? import <nixpkgs> {} }:

let
  # 1. Define the plugins, including nvim-web-devicons for lualine
  neovimPlugins = with pkgs.vimPlugins; [
    # UI and Core
    lualine-nvim
    nvim-web-devicons  # Dependency for lualine icons
    plenary-nvim

    # LSP, Linting, and Formatting
    nvim-lspconfig
    null-ls-nvim

    # Nix-specific enhancements
    vim-nix
  ];

  # 2. Define the entire Neovim configuration as a Lua string
  neovimConfig = ''
    -- Nix-managed Neovim Configuration --

    -- Standard options
    vim.g.mapleader = ' '
    vim.opt.number = true
    vim.opt.relativenumber = true
    vim.opt.termguicolors = true
    vim.opt.mouse = 'a'

    -- Safer way to load plugins
    local function setup_plugin(name)
      local setup_ok, plugin = pcall(require, name)
      if not setup_ok then return nil end
      return plugin
    end

    -- Initialize Lualine
    local lualine = setup_plugin('lualine')
    if lualine then
      lualine.setup({
        options = {
          theme = 'auto',
          icons_enabled = true,
          component_separators = { left = '', right = ''},
          section_separators = { left = '', right = ''},
        }
      })
    end

    -- --- LSP, Linting, and Formatting Setup ---
    local lspconfig = setup_plugin('lspconfig')
    local null_ls = setup_plugin('null-ls')

    -- A. Configure nil (Nix Language Server)
    if lspconfig then lspconfig.nil_ls.setup({}) end

    -- B. Configure null-ls to use our Nix tools
    if null_ls then
      null_ls.setup({
        sources = {
          null_ls.builtins.formatting.alejandra,
          null_ls.builtins.diagnostics.statix,
          null_ls.builtins.diagnostics.deadnix,
        },
      })
    end

    -- C. Enable Auto-formatting on Save for Nix files
    vim.api.nvim_create_autocmd("BufWritePre", {
      pattern = "*.nix",
      callback = function() vim.lsp.buf.format({ async = false }) end,
    })
  '';

  # 3. Build the custom Neovim package using overrideAttrs
  customNeovim = pkgs.neovim.overrideAttrs (oldAttrs: {
    configure = {
      customRC = neovimConfig;
      packages.myPlugins = neovimPlugins;
    };
  });

in
pkgs.mkShell {
  # 4. List all packages required in the shell environment
  buildInputs = [
    customNeovim
  ] ++ (with pkgs; [
    # Core development tools
    git
    ripgrep
    fd

    # External tools for LSP, linting, and formatting
    nil
    alejandra
    statix
    deadnix
    btop
  ]);

  # 5. Isolate the environment AND fix the "Missing user config file" warning
  shellHook = ''
    echo "Entering isolated Neovim shell for Nix development."
    echo "Tools: nil (LSP), alejandra (formatter), statix/deadnix (linters)."
    echo "Launch with: nvim"

    # Set temporary directories to isolate the config
    export XDG_CONFIG_HOME=$(mktemp -d)
    export XDG_DATA_HOME=$(mktemp -d)

    # --- FIX for the 'Missing user config file' warning ---
    # Create an empty init.lua in the temporary config directory.
    # This satisfies Neovim's check, silencing the warning. Our actual
    # config is still loaded perfectly from the Nix derivation.
    mkdir -p "$XDG_CONFIG_HOME/nvim"
    touch "$XDG_CONFIG_HOME/nvim/init.lua"
  '';
}
