{ ... }:

{
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
    tmux = {
      enableShellIntegration = true;
      shellIntegrationOptions = [ "-d 40%" ];
    };
    historyWidgetOptions = [
      "--sort"
      "--exact"
    ];
    fileWidgetCommand = "fd --type f";
  };
}
