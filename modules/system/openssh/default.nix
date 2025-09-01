{ ... }:

{
  services.openssh = {
    enable = true;
    allowSFTP = false;
    ports = [ 47 ];
    settings = {
      PasswordAuthentication = true;
      AllowUsers = [ "ahsan" ];
      AllowGroups = [ "@wheel" ];
      UseDns = false;
      X11Forwarding = false;
      PermitRootLogin = "prohibit-password";
      AllowTcpForwarding = false;
      ClientAliveCountMax = 2;
      LogLevel = "VERBOSE";
      MaxAuthTries = 3;
      MaxSessions = 2;
      TCPKeepAlive = false;
      AllowAgentForwarding = false;
    };
  };
}
