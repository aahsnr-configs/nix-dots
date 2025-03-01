{ config, pkgs, lib, ... }:

{
  boot.kernelPatches = [ {
    name = "crashdump-config";
    patch = null;
    extraConfig = ''
            SECURITY_APPARMOR y
            SECURITY_APPARMOR_INTROSPECT_POLICY y
            SECURITY_APPARMOR_HASH y
            SECURITY_APPARMOR_HASH_DEFAULT y
            SECURITY_APPARMOR_EXPORT_BINARY y
            SECURITY_APPARMOR_PARANOID_LOAD y
            DEFAULT_SECURITY_APPARMOR y
            CC_OPTIMIZE_FOR_PERFORMANCE y
            CPU_FREQ_GOV_PERFORMANCE y
          '';
    } ];
}

