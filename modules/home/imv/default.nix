{ ... }: {
  programs.imv = {
    enable = true;
    settings = { aliases.q = "close"; };
  };
}
