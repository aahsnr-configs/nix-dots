{...}:{
  services.gammastep = {
    enable = true;
    provider = "manual";
    dawnTime = "4:00-5:45";
    duskTime = "18:35-20:15";
    latitude = 24.0;
    longitude = 90.0;
    temperature = {
      day = 4500;
      night = 4500;
    };
    settings = {
      general = {
        adjustment-method = "wayland";
      };
    };
  }; 
}
