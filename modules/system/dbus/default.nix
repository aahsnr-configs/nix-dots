{ pkgs, ... }:

{
  services.dbus = {
    enable = true;
    apparmor = "enabled";
    implementation = "broker";
    brokerPackage = pkgs.dbus-broker;
    dbusPackage = pkgs.dbus
  };


}

