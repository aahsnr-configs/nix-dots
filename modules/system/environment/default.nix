{pkgs, ...}: {
  environment = {
    homeBinInPath = true;
    localBinInPath = true;
    shells = [pkgs.zsh];
    pathsToLink = ["/share/zsh"];
    variables = {
      EDITOR = "nvim";
      BROWSER = "firefox";
      TERMINAL = "kitty";
    };
  };
}
