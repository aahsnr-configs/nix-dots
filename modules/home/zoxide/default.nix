{ ... }: {
  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
    options = [ "--no-cmd" ];
  };
}
