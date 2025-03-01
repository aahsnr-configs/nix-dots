{  pkgs, ... }:

{
    virtualisation.virtualbox = {
        host = {
            enable = true;
            enableExtensionPack = true;
            enableHardening = true;
        };
        guest = {
            vboxsf = true;
            seamless = true;
            dragAndDrop = true;
            clipboard = true;
        };
    };

    users.extraGroups.vboxusers.members = [ "user-with-access-to-virtualbox" ];
}

