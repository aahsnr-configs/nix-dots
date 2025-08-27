{ pkgs, ... }: {
  services.clamav = {
    package = pkgs.clamav;
    scanner = {
      enable = true;
      scanDirectories = [ "/home" "/var/lib" "/tmp" "/etc" "/var/tmp" ];
      interval = "*-*-* 04:00:00";
    };
    updater = {
      enable = true;
      interval = "hourly";
      frequency = 12;
    };
    fangfrisch = {
      enable = true;
      settings = {
        securiteinfo = {
          customer_id = "your customer_id";
          enabled = "yes";
        };
      };
      interval = "hourly";
    };
    daemon.enable = true;
  };
}
