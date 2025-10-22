# keybindings.nix - Custom keybindings for enhanced workflow
{ config, pkgs, lib, ... }:

{
  programs.nixvim = {
    keymaps = [
      # General mappings
      {
        mode = "n";
        key = "<Esc>";
        action = "<cmd>nohlsearch<CR>";
        options = {
          desc = "Clear search highlights";
          silent = true;
        };
      }

      # Better window navigation
      {
        mode = "n";
        key = "<C-h>";
        action = "<C-w>h";
        options = {
          desc = "Move to left window";
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<C-j>";
        action = "<C-w>j";
        options = {
          desc = "Move to bottom window";
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<C-k>";
        action = "<C-w>k";
        options = {
          desc = "Move to top window";
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<C-l>";
        action = "<C-w>l";
        options = {
          desc = "Move to right window";
          silent = true;
        };
      }

      # Resize windows with arrows
      {
        mode = "n";
        key = "<C-Up>";
        action = "<cmd>resize +2<CR>";
        options = {
          desc = "Increase window height";
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<C-Down>";
        action = "<cmd>resize -2<CR>";
        options = {
          desc = "Decrease window height";
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<C-Left>";
        action = "<cmd>vertical resize -2<CR>";
        options = {
          desc = "Decrease window width";
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<C-Right>";
        action = "<cmd>vertical resize +2<CR>";
        options = {
          desc = "Increase window width";
          silent = true;
        };
      }

      # Buffer navigation
      {
        mode = "n";
        key = "<S-h>";
        action = "<cmd>bprevious<CR>";
        options = {
          desc = "Previous buffer";
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<S-l>";
        action = "<cmd>bnext<CR>";
        options = {
          desc = "Next buffer";
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<leader>bd";
        action = "<cmd>Bdelete<CR>";
        options = {
          desc = "Delete buffer";
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<leader>bD";
        action = "<cmd>%bd|e#|bd#<CR>";
        options = {
          desc = "Delete all buffers except current";
          silent = true;
        };
      }

      # Better indenting
      {
        mode = "v";
        key = "<";
        action = "<gv";
        options = {
          desc = "Indent left and reselect";
          silent = true;
        };
      }
      {
        mode = "v";
        key = ">";
        action = ">gv";
        options = {
          desc = "Indent right and reselect";
          silent = true;
        };
      }

      # Move text up and down
      {
        mode = "v";
        key = "J";
        action = ":m '>+1<CR>gv=gv";
        options = {
          desc = "Move text down";
          silent = true;
        };
      }
      {
        mode = "v";
        key = "K";
        action = ":m '<-2<CR>gv=gv";
        options = {
          desc = "Move text up";
          silent = true;
        };
      }

      # Better paste
      {
        mode = "v";
        key = "p";
        action = ''"_dP'';
        options = {
          desc = "Paste without yanking";
          silent = true;
        };
      }

      # Save file
      {
        mode = [ "n" "i" "v" ];
        key = "<C-s>";
        action = "<cmd>w<CR><Esc>";
        options = {
          desc = "Save file";
          silent = true;
        };
      }

      # Quit
      {
        mode = "n";
        key = "<leader>q";
        action = "<cmd>qa<CR>";
        options = {
          desc = "Quit all";
          silent = true;
        };
      }

      # Neo-tree
      {
        mode = "n";
        key = "<leader>e";
        action = "<cmd>Neotree toggle<CR>";
        options = {
          desc = "Toggle file explorer";
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<leader>o";
        action = "<cmd>Neotree focus<CR>";
        options = {
          desc = "Focus file explorer";
          silent = true;
        };
      }

      # Lazy
      {
        mode = "n";
        key = "<leader>L";
        action = "<cmd>Lazy<CR>";
        options = {
          desc = "Open Lazy";
          silent = true;
        };
      }

      # Lazygit
      {
        mode = "n";
        key = "<leader>gg";
        action = "<cmd>LazyGit<CR>";
        options = {
          desc = "Open LazyGit";
          silent = true;
        };
      }

      # Trouble
      {
        mode = "n";
        key = "<leader>xx";
        action = "<cmd>Trouble diagnostics toggle<CR>";
        options = {
          desc = "Toggle Trouble";
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<leader>xX";
        action = "<cmd>Trouble diagnostics toggle filter.buf=0<CR>";
        options = {
          desc = "Buffer diagnostics (Trouble)";
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<leader>cs";
        action = "<cmd>Trouble symbols toggle focus=false<CR>";
        options = {
          desc = "Symbols (Trouble)";
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<leader>cl";
        action = "<cmd>Trouble lsp toggle focus=false win.position=right<CR>";
        options = {
          desc = "LSP Definitions / references / ... (Trouble)";
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<leader>xL";
        action = "<cmd>Trouble loclist toggle<CR>";
        options = {
          desc = "Location List (Trouble)";
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<leader>xQ";
        action = "<cmd>Trouble qflist toggle<CR>";
        options = {
          desc = "Quickfix List (Trouble)";
          silent = true;
        };
      }

      # Todo comments
      {
        mode = "n";
        key = "<leader>ft";
        action = "<cmd>TodoTelescope<CR>";
        options = {
          desc = "Find TODOs";
          silent = true;
        };
      }

      # Markdown preview
      {
        mode = "n";
        key = "<leader>mp";
        action = "<cmd>MarkdownPreview<CR>";
        options = {
          desc = "Markdown Preview";
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<leader>ms";
        action = "<cmd>MarkdownPreviewStop<CR>";
        options = {
          desc = "Stop Markdown Preview";
          silent = true;
        };
      }

      # Python virtual environment
      {
        mode = "n";
        key = "<leader>cv";
        action = "<cmd>VenvSelect<CR>";
        options = {
          desc = "Select Python virtual environment";
          silent = true;
        };
      }

      # Toggle terminal
      {
        mode = "n";
        key = "<leader>tt";
        action = "<cmd>ToggleTerm<CR>";
        options = {
          desc = "Toggle terminal";
          silent = true;
        };
      }

      # LSP Additional keymaps (supplementing the ones in lsp.nix)
      {
        mode = "n";
        key = "<leader>li";
        action = "<cmd>LspInfo<CR>";
        options = {
          desc = "LSP Info";
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<leader>lI";
        action = "<cmd>Mason<CR>";
        options = {
          desc = "Mason Info";
          silent = true;
        };
      }

      # Format
      {
        mode = "n";
        key = "<leader>cf";
        action = "<cmd>lua vim.lsp.buf.format()<CR>";
        options = {
          desc = "Format buffer";
          silent = true;
        };
      }

      # Telescope additional keymaps
      {
        mode = "n";
        key = "<leader>fs";
        action = "<cmd>Telescope lsp_document_symbols<CR>";
        options = {
          desc = "Document symbols";
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<leader>fS";
        action = "<cmd>Telescope lsp_workspace_symbols<CR>";
        options = {
          desc = "Workspace symbols";
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<leader>fd";
        action = "<cmd>Telescope diagnostics<CR>";
        options = {
          desc = "Workspace diagnostics";
          silent = true;
        };
      }

      # Split navigation
      {
        mode = "n";
        key = "<leader>sv";
        action = "<cmd>vsplit<CR>";
        options = {
          desc = "Vertical split";
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<leader>sh";
        action = "<cmd>split<CR>";
        options = {
          desc = "Horizontal split";
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<leader>sc";
        action = "<cmd>close<CR>";
        options = {
          desc = "Close split";
          silent = true;
        };
      }

      # Center screen when scrolling
      {
        mode = "n";
        key = "<C-d>";
        action = "<C-d>zz";
        options = {
          desc = "Scroll down and center";
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<C-u>";
        action = "<C-u>zz";
        options = {
          desc = "Scroll up and center";
          silent = true;
        };
      }
      {
        mode = "n";
        key = "n";
        action = "nzzzv";
        options = {
          desc = "Next search result and center";
          silent = true;
        };
      }
      {
        mode = "n";
        key = "N";
        action = "Nzzzv";
        options = {
          desc = "Previous search result and center";
          silent = true;
        };
      }

      # Quick fix navigation
      {
        mode = "n";
        key = "]q";
        action = "<cmd>cnext<CR>";
        options = {
          desc = "Next quickfix item";
          silent = true;
        };
      }
      {
        mode = "n";
        key = "[q";
        action = "<cmd>cprev<CR>";
        options = {
          desc = "Previous quickfix item";
          silent = true;
        };
      }

      # Select all
      {
        mode = "n";
        key = "<C-a>";
        action = "ggVG";
        options = {
          desc = "Select all";
          silent = true;
        };
      }
    ];
  };
}
