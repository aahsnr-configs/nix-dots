{ pkgs, ...}:{
  programs.brave.nativeMessagingHosts = [ pkgs.kdePackages.plasma-browser-integration ];  
}


