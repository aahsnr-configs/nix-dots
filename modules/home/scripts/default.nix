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
    safe-rm = mkFishScript {
      name = "safe-rm";
      text = builtins.readFile ./safe-rm.fish;
    };

    nuke-nvim = mkFishScript {
      name = "nuke-nvim";
      text = builtins.readFile ./nuke-nvim.fish;
    };

    setup-github-keys = mkFishScript {
      name = "setup-github-keys";
      text = builtins.readFile ./setup-github-keys.fish;
      runtimeInputs = [ pkgs.openssh pkgs.gh ];
    };

    launch-first-available = mkFishScript {
      name = "launch_first_available";
      text = builtins.readFile ./launch_first_available.fish;
    };

    fuzzel-emoji = mkFishScript {
      name = "fuzzel-emoji";
      text = builtins.readFile ./fuzzel-emoji.fish;
      runtimeInputs = [ pkgs.fuzzel pkgs.wtype pkgs.wl-clipboard ];
    };

    wsaction = mkFishScript {
      name = "wsaction";
      text = builtins.readFile ./wsaction.fish;
      runtimeInputs = [ pkgs.hyprland pkgs.jq ];
    };

    fe = mkFishScript {
      name = "fe";
      text = builtins.readFile ./fe.fish;
      runtimeInputs = [ pkgs.fd pkgs.fzf pkgs.bat ];
    };

    se = mkFishScript {
      name = "se";
      text = builtins.readFile ./se.fish;
      runtimeInputs = [ pkgs.ripgrep pkgs.fzf pkgs.bat ];
    };

    fkill = mkFishScript {
      name = "fkill";
      text = builtins.readFile ./fkill.fish;
      runtimeInputs = [ pkgs.fzf pkgs.procps pkgs.gawk ];
    };

    fconf = mkFishScript {
      name = "fconf";
      text = builtins.readFile ./fconf.fish;
      runtimeInputs = [ pkgs.fd pkgs.fzf pkgs.bat ];
    };

    fp = mkFishScript {
      name = "fp";
      text = builtins.readFile ./fp.fish;
      runtimeInputs = [ pkgs.fd pkgs.fzf pkgs.eza pkgs.tree pkgs.chafa pkgs.bat pkgs.file ];
    };

    fssh = mkFishScript {
      name = "fssh";
      text = builtins.readFile ./fssh.fish;
      runtimeInputs = [ pkgs.fzf pkgs.openssh pkgs.gawk ];
    };

    org-capture = mkFishScript {
      name = "org-capture";
      text = builtins.readFile ./org-capture.fish;
    };
  };

in
{
  # Install all the scripts defined above into the user's environment.
  home.packages = pkgs.lib.attrValues scripts;
}
