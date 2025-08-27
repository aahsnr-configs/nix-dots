{ config, lib, pkgs, ... }:
let
  inherit (lib) versionOlder;

  # Use the latest possible nvidia package
  nvStable = config.boot.kernelPackages.nvidiaPackages.stable.version;
  nvBeta = config.boot.kernelPackages.nvidiaPackages.beta.version;

  nvidiaPackage = if (versionOlder nvBeta nvStable) then
    config.boot.kernelPackages.nvidiaPackages.stable
  else
    config.boot.kernelPackages.nvidiaPackages.beta;
in {
  boot = {
    kernelModules = [ "nvidia" "nvidia_modeset" "nvidia_drm" "nvidia_uvm" ];

    blacklistedKernelModules = [ "nouveau" ];
  };

  environment = {
    sessionVariables = {
      GBM_BACKEND = "nvidia-drm";
      LIBVA_DRIVER_NAME = "nvidia";
      __GLX_VENDOR_LIBRARY_NAME = "nvidia";
      NVD_BACKEND = "direct";
    };

    systemPackages = with pkgs; [
      vulkan-loader
      vulkan-validation-layers
      vulkan-tools
      vaapiVdpau
      libvdpau-va-gl
      xwayland
      nvidia-vaapi-driver
    ];
  };

  hardware = {
    brillo.enable = false;
    graphics = {
      enable = true;
      enable32Bit = true;
    };
    nvidia = {
      package = nvidiaPackage;
      open = true;
      powerManagement.enable = true;
      dynamicBoost.enable = true;
      modesetting.enable = true;
    };
    nvidia-container-toolkit.enable = true;
  };
}
