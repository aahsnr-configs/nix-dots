{...}: {
  services = {
    logind = {
      lidSwitch = "suspend";
      lidSwitchExternalPower = "hibernate";
      powerKey = "lock";
      powerKeyLongPress = "poweroff";
      rebootKey = "reboot";
      settings.Login = {
        HandlePowerKey = "poweroff";
        HibernateDelaySec = 600;
        SuspendState = "mem";
      };
    };
  };
}
