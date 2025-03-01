{ ... }:

{
  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [
      "C.UTF-8/UTF-8"
      "en_US.UTF-8/UTF-8"
    ];
    #Changing the option below can disable handling of i18n.defaultLocale and supportedLocale.
    #glibcLocales = pkgs.glibcLocales;
    extraLocaleSettings = {
      LC_ADDRESS = "en_US";
      LC_IDENTIFICATION = "en_US";
      LC_MEASUREMENT = "en_US";
      LC_MONETARY = "en_US";
      LC_NAME = "en_US";
      LC_NUMERIC = "en_US";
      LC_PAPER = "en_US";
      LC_TELEPHONE = "en_US";
      LC_TIME = "en_US";
    };
  };
}

