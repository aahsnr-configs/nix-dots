{ pkgs, ... }:
{

  hardware.ledger.enable = true;

  systemd.coredump.enable = false;

  services = {
    jitterentropy-rngd.enable = true;
    haveged.enable = true;
    sysstat.enable = true;
    networkd-dispatcher.enable = true;
    #dbus.apparmor = "enabled";
  };

  programs.atop.atopacctService.enable = true;

  security = {
    protectKernelImage = false;
    lockKernelModules = false;
    allowSimultaneousMultithreading = true;
    polkit = {
      enable = true;
      extraConfig = ''
          polkit.addRule(function(action, subject) {
          if (
            subject.isInGroup("users")
              && (
                action.id == "org.freedesktop.login1.reboot" ||
                action.id == "org.freedesktop.login1.reboot-multiple-sessions" ||
                action.id == "org.freedesktop.login1.power-off" ||
                action.id == "org.freedesktop.login1.power-off-multiple-sessions"
              )
            )
          {
            return polkit.Result.YES;
          }
        })
      '';
    };
    rtkit.enable = true;
    virtualisation.flushL1DataCache = "always";
    forcePageTableIsolation = true;
    sudo.package = pkgs.sudo.override { withInsults = true; };
    apparmor = {
      enable = true;
      enableCache = true;
      killUnconfinedConfinables = true;
      packages = with pkgs; [
        apparmor-pam
        apparmor-utils
        apparmor-parser
        apparmor-profiles
        apparmor-bin-utils
        #apparmor-kernel-patches
        #roddhjav-apparmor-rules
        libapparmor
      ];
    };
    audit = {
      enable = true;
      rules = [ "-a exit,always -F arch=b64 -S execve" ];
      backlogLimit = 32;
      failureMode = "printk";
    };
    auditd.enable = true;
    pam = {
      loginLimits = [
        {
          domain = "*";
          item = "core";
          type = "hard";
          value = "0";
        }
      ];
      # services = {
      #   # hyprlock = { };
      #   greetd.enableGnomeKeyring = true;
      # };
    };
  };
}
