# core.nix - Core Neovim settings and sensible defaults
{ config, pkgs, lib, ... }:

{
  programs.nixvim = {
    # Global options for a modern editing experience
    opts = {
      # Line numbers
      number = true;
      relativenumber = false;

      # Indentation
      tabstop = 4;
      shiftwidth = 4;
      softtabstop = 4;
      expandtab = true;
      smartindent = true;
      autoindent = true;

      # Search
      ignorecase = true;
      smartcase = true;
      hlsearch = false;
      incsearch = true;

      # UI improvements
      termguicolors = true;
      signcolumn = "yes";
      scrolloff = 8;
      sidescrolloff = 8;
      wrap = true;
      showmode = false;

      # Split behavior
      splitbelow = true;
      splitright = true;

      # Persistent undo
      undofile = true;
      undolevels = 10000;
      
      # Better backup/swap file handling
      backup = false;
      writebackup = false;
      swapfile = false;

      # Update time (affects CursorHold and swap file writing)
      updatetime = 250;
      
      # Time in milliseconds to wait for a mapped sequence
      timeoutlen = 300;

      # Completion menu
      completeopt = "menu,menuone,noselect";
      
      # Better display for messages
      cmdheight = 1;
      
      # Show which line your cursor is on
      cursorline = true;
      
      # Enable mouse support
      mouse = "a";
      
      # Clipboard integration
      clipboard = "unnamedplus";
      
      # Better editing experience
      conceallevel = 0;
      
      # Folding
      foldmethod = "expr";
      foldexpr = "nvim_treesitter#foldexpr()";
      foldenable = false; # Start with folds open
      foldlevel = 99;
    };

    # Global variables
    globals = {
      mapleader = " ";
      maplocalleader = " ";
      
      # Disable some built-in plugins we don't need
      loaded_netrw = 1;
      loaded_netrwPlugin = 1;
      loaded_gzip = 1;
      loaded_zipPlugin = 1;
      loaded_tarPlugin = 1;
      loaded_2html_plugin = 1;
    };

    # Auto commands for quality of life improvements
    autoCmd = [
      # Highlight on yank
      {
        event = [ "TextYankPost" ];
        pattern = [ "*" ];
        callback = {
          __raw = ''
            function()
              vim.highlight.on_yank({ timeout = 200 })
            end
          '';
        };
      }
      
      # Remove trailing whitespace on save
      {
        event = [ "BufWritePre" ];
        pattern = [ "*" ];
        command = "%s/\\s\\+$//e";
      }
      
      # Return to last edit position when opening files
      {
        event = [ "BufReadPost" ];
        pattern = [ "*" ];
        callback = {
          __raw = ''
            function()
              if vim.fn.line("'\"") > 0 and vim.fn.line("'\"") <= vim.fn.line("$") then
                vim.fn.setpos('.', vim.fn.getpos("'\""))
                vim.cmd('silent! foldopen')
              end
            end
          '';
        };
      }
      
      # Close some filetypes with <q>
      {
        event = [ "FileType" ];
        pattern = [ "qf" "help" "man" "notify" "lspinfo" "spectre_panel" ];
        callback = {
          __raw = ''
            function()
              vim.keymap.set("n", "q", "<cmd>close<cr>", { buffer = true })
            end
          '';
        };
      }
    ];
  };
}
