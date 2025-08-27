{...}: {
  services = {
    logind = {
      lidSwitch = "suspend";
      lidSwitchExternalPower = "hibernate";
      powerKey = "lock";
      powerKeyLongPress = "poweroff";
      rebootKey = "reboot";
      extraConfig = ''
        HandlePowerKey=poweroff
        HibernateDelaySec=600
        SuspendState=mem
      '';
    };
  };
}
