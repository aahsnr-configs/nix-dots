# Example home.nix - How to integrate this Nixvim configuration

{ config, pkgs, lib, ... }:

{

# Import the Nixvim configuration

imports = [ # Option 1: Direct import (if you have the files locally)
./nixvim/default.nix

    # Option 2: From flake input (add to your flake.nix first)
    # inputs.nixvim-config.homeManagerModules.default

];

# Your home-manager configuration

home = {
username = "your-username";
homeDirectory = "/home/your-username";
stateVersion = "24.05";

    # Additional packages you might want
    packages = with pkgs; [
      # Development tools
      git
      gh
      lazygit

      # Language tools (these are included in the nixvim config but you might want them globally)
      python3
      nodejs

      # Fonts (highly recommended for icons to work)
      (nerdfonts.override { fonts = [ "JetBrainsMono" "FiraCode" "Hack" ]; })
    ];

    # Session variables
    sessionVariables = {
      EDITOR = "nvim";
      VISUAL = "nvim";
    };

};

# Font configuration (optional but recommended for icons)

fonts.fontconfig.enable = true;

# Optional: Override or extend the Nixvim configuration

programs.nixvim = { # You can add additional configuration here # These will be merged with the imported configuration

    # Example: Add extra plugins
    # plugins.harpoon.enable = true;

    # Example: Override settings
    # opts.tabstop = 2;
    # opts.shiftwidth = 2;

    # Example: Add custom key mappings
    # keymaps = [
    #   {
    #     mode = "n";
    #     key = "<leader>w";
    #     action = "<cmd>write<CR>";
    #     options.desc = "Quick save";
    #   }
    # ];

};

# Git configuration (works well with the Git plugins)

programs.git = {
enable = true;
userName = "Your Name";
userEmail = "your.email@example.com";
extraConfig = {
init.defaultBranch = "main";
pull.rebase = true;
};
};

# Shell configuration

programs.bash = {
enable = true;
shellAliases = {
vim = "nvim";
vi = "nvim";
v = "nvim";
};
};

# Optional: Zsh configuration

programs.zsh = {
enable = true;
shellAliases = {
vim = "nvim";
vi = "nvim";
v = "nvim";
};
oh-my-zsh = {
enable = true;
theme = "robbyrussell";
plugins = [ "git" "python" "docker" ];
};
};

# Let Home Manager install and manage itself

programs.home-manager.enable = true;
}
