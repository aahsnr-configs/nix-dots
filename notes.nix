ome.file.".config/hypr/hyprland.conf".source = ./hyprland.conf;
{
  packageOverrides = pkgs: rec {
    foo = pkgs.foo.override { doCheck = false; doInstallCheck = false; };
  };
}

{
  packageOverrides = pkgs: with pkgs; rec {
    myProfile = writeText "my-profile" ''
      export PATH=$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:/sbin:/bin:/usr/sbin:/usr/bin
      export MANPATH=$HOME/.nix-profile/share/man:/nix/var/nix/profiles/default/share/man:/usr/share/man
    '';
    myPackages = pkgs.buildEnv {
      name = "my-packages";
      paths = [
        (runCommand "profile" {} ''
          mkdir -p $out/etc/profile.d
          cp ${myProfile} $out/etc/profile.d/my-profile.sh
        '')
        aspell
        bc
        coreutils
        ffmpeg
        man
        nixUnstable
        emscripten
        jq
        nox
        silver-searcher
      ];
      pathsToLink = [ "/share/man" "/share/doc" "/bin" "/etc" ];
      extraOutputsToInstall = [ "man" "doc" ];
    };
  };
}

 boot.kernelParams = [ "security=selinux" ];
 # compile kernel with SELinux support - but also support for other LSM modules
 boot.kernelPatches = [ {
        name = "selinux-config";
        patch = null;
        extraConfig = ''
                SECURITY_SELINUX y
                SECURITY_SELINUX_BOOTPARAM n
                SECURITY_SELINUX_DISABLE n
                SECURITY_SELINUX_DEVELOP y
                SECURITY_SELINUX_AVC_STATS y
                SECURITY_SELINUX_CHECKREQPROT_VALUE 0
                DEFAULT_SECURITY_SELINUX n
              '';
        } ];
 # policycoreutils is for load_policy, fixfiles, setfiles, setsebool, semodile, and sestatus.
 environment.systemPackages = with pkgs; [ policycoreutils ];
 # build systemd with SELinux support so it loads policy at boot and supports file labelling
 systemd.package = pkgs.systemd.override { withSelinux = true; };

@spacegreg, the only profiles from apparmor-profiles which have been adapted to NixOS so far are those in abstractions/ and tunables/, see nixos/modules/security/apparmor/includes.nix, they’re commonly used with the include keyword in applications’ profiles (eg. include <abstractions/base>).

Most of the content of profiles should be written in Nixpkgs’ pkgs/ in a postInstall defining an apparmor output. Those profiles use includes and apparmorRulesFromClosure as much as possible, and include a local/ customization (eg. include <local/bin.transmission-daemon>) which will then be optionally created by NixOS to add rules to the profile if need be (in this example, by setting security.apparmor.includes."local/bin.transmission-daemon").

Currently application profiles should be defined using security.apparmor.policies."bin.transmission-daemon".profile to include the profile from Nixpkgs (eg. include "${pkgs.transmission.apparmor}/bin.transmission-daemon"), but thinking about it, we should maybe load those profiles from environment.systemPackages like .desktop files, using environment.pathsToLink and environment.extraSetup.

The term policies in security.apparmor.policies does not really match something in AppArmor, I just needed a term to gather under the same name, the three options enable, enforce (whether to forbid or just report unallowed access), and profile (the rules for some executable paths).

{ config, pkgs, ... }:
let
  tex = (pkgs.texlive.combine {
    inherit (pkgs.texlive) scheme-basic
      dvisvgm dvipng # for preview and export as html
      wrapfig amsmath ulem hyperref capt-of;
      #(setq org-latex-compiler "lualatex")
      #(setq org-preview-latex-default-process 'dvisvgm)
  });
in
{ # home-manager
  home.packages = with pkgs; [
    tex
  ];
}

Attribute sets declared with rec prepended allow access to attributes from within the set.

rec {
  one = 1;
  two = one + 1;
  three = two + 1;
}


The value of a Nix expression can be inserted into a character string with the dollar-sign and braces (${ }).

let
  name = "Nix";
in
"hello ${name}"


