{ ... }: {
  # rtkit is optional but recommended
  # rtkit is already set in security system module
  services = {
    pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      pulse.enable = true;
      wireplumber.enable = true;
    };
    pulseaudio.enable = false;
  };
}
