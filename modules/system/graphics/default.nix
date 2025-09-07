{ config, pkgs, ... }:
{
  boot = {
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
      package = config.boot.kernelPackages.nvidiaPackages.latest;
      open = true;
      powerManagement.enable = false;
      dynamicBoost.enable = false;
      modesetting.enable = true;
      nvidiaSettings = true;
    };
    nvidia-container-toolkit.enable = true;
  };
}
