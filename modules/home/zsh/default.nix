{ config, lib, pkgs, ...}:

{
  home.sessionVariables.STARSHIP_CACHE = "${config.xdg.cacheHome}/starship";

  programs.zsh = {
    enable = true;
    autosuggestion = {
      enable = true;
      strategy = [ "history" ];
    };
    enableVteIntegration = true;
    enableCompletion = true;
    syntaxHighlighting = {
      enable = true;
      highlighters = ["brackets"];
    };
    sessionVariables = {
      LC_ALL = "en_US.UTF-8";
      ZSH_AUTOSUGGEST_USE_ASYNC = "true";
    };
    completionInit = "autoload -U compinit && compinit";
    history = {
      save = 2137;
      size = 2137;
      expireDuplicatesFirst = true;
      ignoreDups = false;
      ignoreSpace = true;
    };

    dirHashes = {
      docs = "$HOME/Documents";
      notes = "$HOME/Documents/Notes";
      dots = "$HOME/Dev/nix-dots";
      dl = "$HOME/Download";
      vids = "$HOME/Videos";
      music = "$HOME/Music";
      media = "/run/media/$USER";
    };
    shellAliases = import ./aliases.nix {inherit pkgs lib config;};
    plugins = [
      {
        name = "zsh-nix-shell";
        file = "nix-shell.plugin.zsh";
        src = pkgs.fetchFromGitHub {
          owner = "chisui";
          repo = "zsh-nix-shell";
          rev = "82ca15e638cc208e6d8368e34a1625ed75e08f90";
          sha256 = "Rtg8kWVLhXRuD2/Ctbtgz9MQCtKZOLpAIdommZhXKdE=";
        };
      }
      {
        name = "fzf-tab";
        file = "fzf-tab.plugin.zsh";
        src = pkgs.fetchFromGitHub {
          owner = "Aloxaf";
          repo = "fzf-tab";
          rev = "01dad759c4466600b639b442ca24aebd5178e799";
          sha256 = "q26XVS/LcyZPRqDNwKKA9exgBByE0muyuNb0Bbar2lY=";
        };
      }
    ];
  };

  # home = {
  #   packages = with pkgs; [
  #     zsh-fzf-tab
  #     zsh-nix-shell
  #     zsh-autopair
  #     nix-zsh-completions
  #     zsh-history-substring-search
  #   ];
  # };


}
