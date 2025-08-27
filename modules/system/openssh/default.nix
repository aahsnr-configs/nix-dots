{...}: {
  services.openssh = {
    enable = true;
    allowSFTP = false;
    ports = [47];
    settings = {
      PasswordAuthentication = true;
      AllowUsers = ["ahsan"]; # Allows all users by default. Can be [ "user1" "user2" ]
      AllowGroups = ["@wheel"];
      UseDns = false;
      X11Forwarding = false;
      PermitRootLogin = "prohibit-password"; # "yes", "without-password", "prohibit-password", "forced-commands-only", "no"
    };
  };
}
