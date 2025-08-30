# /etc/nixos/configuration.nix

{ config, pkgs, lib, ... }:

{
  # Enable AppArmor
  security.apparmor = {
    enable = true;
    
    # Enable caching of AppArmor policies
    enableCache = true;
    
    # Add packages containing AppArmor abstractions and tunables
    packages = with pkgs; [
      apparmor-profiles       # Standard AppArmor profiles (limited, mostly abstractions)
      roddhjav-apparmor-rules # Over 1500 AppArmor profiles from apparmor.d project
    ];
    
    # Override abstractions to work with Nix store
    includes = {
      "abstractions/base" = ''
        # Allow read and mmap access to Nix store
        /nix/store/*/bin/** mr,
        /nix/store/*/lib/** mr,
        /nix/store/** r,
      '';
    };
  };
  
  # Enable audit logging for AppArmor debugging
  security.auditd.enable = true;
  
  # Install AppArmor utilities
  environment.systemPackages = with pkgs; [
    apparmor-utils
    apparmor-profiles
    roddhjav-apparmor-rules
  ];
  
  # Manual approach to load specific profiles from both packages
  # Since automatic loading doesn't work, you need to copy profiles manually
  environment.etc = {
    # Profiles from apparmor-profiles package
    "apparmor.d/ping" = {
      source = "${pkgs.apparmor-profiles}/share/apparmor/extra-profiles/ping";
    };
    
    # Profiles from roddhjav-apparmor-rules package
    # Note: These profiles expect FHS paths and will likely need modification
    "apparmor.d/firefox" = {
      source = "${pkgs.roddhjav-apparmor-rules}/etc/apparmor.d/groups/browsers/firefox";
    };
    
    "apparmor.d/chromium" = {
      source = "${pkgs.roddhjav-apparmor-rules}/etc/apparmor.d/groups/browsers/chromium";
    };
    
    # Copy abstractions and tunables from roddhjav-apparmor-rules
    # Note: environment.etc doesn't support recursive copying of directories
    # You'll need to copy individual files or use a different approach
    
    # Alternative: Use a script to copy directories
    # "apparmor.d/copy-roddhjav-rules" = {
    #   source = pkgs.writeScript "copy-roddhjav-rules" ''
    #     #!/bin/sh
    #     cp -r ${pkgs.roddhjav-apparmor-rules}/etc/apparmor.d/* /etc/apparmor.d/
    #   '';
    #   mode = "0755";
    # };
    
    # Example: Copy additional profiles as needed
    # You can explore available profiles with:
    # ls -la /nix/store/*-roddhjav-apparmor-rules*/etc/apparmor.d/
    
    # Note: Most profiles will need path adjustments to work with NixOS
    # You may need to create modified versions instead of direct copies
  };
  
  # Optional: Disable AppArmor escape hatch
  specialisation.no-apparmor.configuration = {
    security.apparmor.enable = lib.mkForce false;
  };
}

# IMPORTANT NOTES:
#
# 1. Current Status of Profile Packages:
#    - Both apparmor-profiles and roddhjav-apparmor-rules have significant issues on NixOS
#    - Profiles expect FHS paths (/usr/bin, /bin) but NixOS uses /nix/store paths
#    - Adding packages to security.apparmor.packages only makes abstractions/tunables available
#    - Profiles are NOT automatically loaded and need manual copying/modification
#
# 2. roddhjav-apparmor-rules package:
#    - Contains over 1500 AppArmor profiles from the apparmor.d project
#    - Profiles are located in /etc/apparmor.d/ within the package
#    - Most profiles need path adjustments to work with NixOS
#
# 3. To explore available profiles:
#    apparmor-profiles: ls /nix/store/*-apparmor-profiles*/share/apparmor/extra-profiles/
#    roddhjav-apparmor-rules: ls /nix/store/*-roddhjav-apparmor-rules*/etc/apparmor.d/
#
# 4. To check which profiles are loaded:
#    sudo aa-status
#
# 5. To manually load a profile:
#    sudo apparmor_parser -r /etc/apparmor.d/PROFILE_NAME
#
# 6. For debugging:
#    sudo tail -f /var/log/audit/audit.log
#
# 7. Known Issues (from nixpkgs PR #359817):
#    - Rule packages are "completely broken" due to FHS path expectations
#    - Build system for rules is ignored in current packaging
#    - Significant work needed to make them compatible with NixOS
