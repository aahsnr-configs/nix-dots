{ pkgs, ... }:

{
  programs.mpv = {
    enable = true;
    scripts = with pkgs.mpvScripts; [mpris thumbnail sponsorblock];
    config = {
      gpu-context = "wayland";
      hwdec = "auto";
      osc = "no";
      profile = "gpu-hq";
      vo = "gpu";
    };
  };
}
