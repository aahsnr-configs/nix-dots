{ ... }:

{
  services = {
    logind = {
      settings.Login = {
        HandleLidSwitch = "suspend";
        HandleLidSwitchExternalPower = "hibernate";
        HandleRebootKey = "reboot";
        HandlePowerKey = "lock";
        HandlePowerKeyLongPress = "poweroff";
        HibernateDelaySec = 600;
        SuspendState = "mem";
      };
    };
  };
}
