{
  config,
  pkgs,
  ...
}: {
  hardware = {
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
      enable32Bit = true;
    };
    cpu.amd.updateMicrocode = true;
    amdgpu.initrd.enable = false;
    nvidia = {
      package = config.boot.kernelPackages.nvidiaPackages.production;
      open = true;
      modesetting.enable = true;
      prime.sync.enable = true;
      nvidiaSettings = true;
      # only if prime.offload is enabled
      # prime = {
      #   amdgpuBusId = "PCI:4:0:0";
      #   nvidiaBusId = "PCI:1:0:0";
      # };
    };
  };

  boot = {
    blacklistedKernelModules = ["nouveau"];
  };

  services = {
    xserver.videoDrivers = [
      "modesetting"
      "nvidia"
      "amdgpu"
    ];
    fstrim.enable = true;
    power-profiles-daemon.enable = true;
    asusd.enable = true;
    hdapsd.enable = true;

    udev.extraHwdb = ''
      evdev:name:*:dmi:bvn*:bvr*:bd*:svnASUS*:pn*:*
       KEYBOARD_KEY_ff31007c=f20    # fixes mic mute button
       KEYBOARD_KEY_ff3100b2=home   # Set fn+LeftArrow as Home
       KEYBOARD_KEY_ff3100b3=end    # Set fn+RightArrow as End
    '';
  };
}
