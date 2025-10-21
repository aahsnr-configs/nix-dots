{ pkgs, ... }:

let
  # A helper function to create a Fish script as a Nix package.
  # Uses writeTextFile instead of writeShellApplication since Fish isn't bash-compatible
  mkFishScript = { name, text, runtimeInputs ? [ ] }:
    let
      # Create the wrapped script with proper PATH setup
      wrappedText = ''
        #!${pkgs.fish}/bin/fish
        ${pkgs.lib.optionalString (runtimeInputs != [ ]) ''
          set -gx PATH ${pkgs.lib.makeBinPath runtimeInputs} $PATH
        ''}
        ${text}
      '';
    in
    pkgs.writeTextFile {
      inherit name;
      text = wrappedText;
      executable = true;
      destination = "/bin/${name}";
      checkPhase = ''
        ${pkgs.fish}/bin/fish -n $out/bin/${name}
      '';
    };

  # All scripts are defined here, reading their content from the ./scripts directory.
  scripts = {
    fconf = mkFishScript {
      name = "fconf";
      text = builtins.readFile ./bin/fconf.fish;
    };

    fe = mkFishScript {
      name = "fe";
      text = builtins.readFile ./bin/fe.fish;
    };

    fkill = mkFishScript {
      name = "fkill";
      text = builtins.readFile ./bin/fkill.fish;
      runtimeInputs = [ pkgs.openssh pkgs.gh ];
    };

    fp = mkFishScript {
      name = "fp";
      text = builtins.readFile ./bin/fp.fish;
    };

    fssh = mkFishScript {
      name = "fssh";
      text = builtins.readFile ./bin/fssh.fish;
      runtimeInputs = [ pkgs.fuzzel pkgs.wtype pkgs.wl-clipboard ];
    };

    fuzzel-emoji = mkFishScript {
      name = "fuzzel-emoji";
      text = builtins.readFile ./bin/fuzzel-emoji.fish;
      runtimeInputs = [ pkgs.hyprland pkgs.jq ];
    };

    launch_first_available = mkFishScript {
      name = "launch_first_available";
      text = builtins.readFile ./bin/launch_first_available.fish;
      runtimeInputs = [ pkgs.fd pkgs.fzf pkgs.bat ];
    };

    nuke-nvim = mkFishScript {
      name = "nuke-nvim";
      text = builtins.readFile ./bin/nuke-nvim.fish;
      runtimeInputs = [ pkgs.ripgrep pkgs.fzf pkgs.bat ];
    };

    org-capture = mkFishScript {
      name = "org-capture";
      text = builtins.readFile ./bin/org-capture.fish;
      runtimeInputs = [ pkgs.fzf pkgs.procps pkgs.gawk ];
    };

    safe-rm = mkFishScript {
      name = "safe-rm";
      text = builtins.readFile ./bin/safe-rm.fish;
      runtimeInputs = [ pkgs.fd pkgs.fzf pkgs.bat ];
    };

    se = mkFishScript {
      name = "se";
      text = builtins.readFile ./bin/se.fish;
      runtimeInputs = [ pkgs.fd pkgs.fzf pkgs.eza pkgs.tree pkgs.chafa pkgs.bat pkgs.file ];
    };

    setup-github-keys = mkFishScript {
      name = "setup-github-keys";
      text = builtins.readFile ./bin/setup-github-keys.fish;
      runtimeInputs = [ pkgs.fzf pkgs.openssh pkgs.gawk ];
    };

    wsaction = mkFishScript {
      name = "wsaction";
      text = builtins.readFile ./bin/wsaction.fish;
      runtimeInputs = [ pkgs.fzf pkgs.openssh pkgs.gawk ];
    };
  };

in
{
  # Install all the scripts defined above into the user's environment.
  home.packages = pkgs.lib.attrValues scripts;
}
