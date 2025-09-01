{ pkgs, ... }:
{
  services.dbus = {
    enable = true;
    implementation = "broker";
    brokerPackage = pkgs.dbus-broker;
    dbusPackage = pkgs.dbus;
  };
}
