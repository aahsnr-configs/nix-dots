{
  config,
  pkgs,
  ...
}: {
  boot = {
    blacklistedKernelModules = ["nouveau"];
  };

  environment = {
    systemPackages = with pkgs; [
      vulkan-loader
      vulkan-validation-layers
      vulkan-tools
      xwayland
      nvidia-vaapi-driver
    ];
  };

  hardware = {
    brillo.enable = false;
    graphics = {
      enable = true;
      extraPackages = with pkgs; [
        vulkan-loader
        vulkan-validation-layers
        vulkan-tools
        xwayland
        nvidia-vaapi-driver
        mesa
      ];
      # enable32Bit = true;
    };
    nvidia = {
      package = config.boot.kernelPackages.nvidiaPackages.latest;
      open = true;
      # testing if the following 2 options fix cpu temp stuck at 82
      powerManagement.enable = true;
      dynamicBoost.enable = true;
      modesetting.enable = true;
      nvidiaSettings = true;
    };
  };
}
