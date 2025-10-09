{ pkgs, ... }:
{
  users.users.root.initialPassword = "changeme";
  users.users.ahsan = {
    isNormalUser = true;
    uid = 1000;
    description = "Ahsanur Rahman";
    extraGroups = [
      "wheel"
      "audio"
      "video"
      "input"
      "network"
      "networkmanager"
      "plugdev"
      "libvirtd"
      "mysql"
      "docker"
      "podman"
      "git"
    ];
    useDefaultShell = true;
    shell = pkgs.fish;
    initialPassword = "changeme";
    packages = with pkgs; [ git ];
  };
   programs.fish.enable = true;
}
