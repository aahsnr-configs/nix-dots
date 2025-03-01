{  pkgs, ... }:

{
    virtualisation = {
      containers.enable = true;
      podman = {
        enable = true;
        autoPrune.enable = true;
        autoPrune.flags = [ "--all" ];
        defaultNetwork.settings = { dns_enabled = true; };
        dockerCompat = true;
        dockerSocket.enable = true;
      };
    };

    environment.systemPackages = with pkgs; [
        boxbuddy
        distrobox
        docker-compose
        dive
        podman-tui
        pods
        podman-compose
    ];

}

