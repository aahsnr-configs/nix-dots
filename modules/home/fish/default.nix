# ~/nix-dots/modules/home/fish/default.nix
{pkgs, ...}: {
  # Install all required packages and shell plugins
  home.packages = with pkgs; [
    fishPlugins.autopair
  ];

  # Configure Fish shell and its ecosystem
  programs.fish = {
    enable = true;
    shellAliases = {
      # General
      gg = "lazygit";
      cat = "bat --paging=never";
      grep = "rg";
      du = "dust";
      rmi = "safe-rm";

      # NixOS / Home Manager
      nixswitch = "sudo nixos-rebuild switch --flake ~/nix-dots#zephyrus";
      nixup = "sudo nixos-rebuild switch --flake ~/nix-dots#zephyrus --upgrade";
      nixs = "nix-shell -p";
      nixgc = "sudo nix-collect-garbage -d";

      # Systemd
      sctl = "systemctl";
      sctle = "sudo systemctl enable";
      sctls = "sudo systemctl start";
    };

    functions = {
      "tldr-fzf" = "tldr --list | fzf --preview 'tldr {1}' --preview-window right:70%";
      yy = ''
        set tmp (mktemp -t "yazi-cwd.XXXXXX")
        yazi $argv --cwd-file="$tmp"

        # Use `tail -n 1` to robustly get the last line from the file,
        # ignoring any extra verbose output Yazi might have added.
        if set cwd (tail -n 1 -- "$tmp"); and test -n "$cwd"; and test "$cwd" != "$PWD"
          cd -- "$cwd"
        end

        rm -f -- "$tmp"
      '';
      rgfzf = ''
        rg --color=always --line-number --no-heading $argv | fzf --ansi \
            --delimiter ':' \
            --preview 'bat --style=numbers --color=always --highlight-line {2} {1}' \
            --preview-window 'right:60%:wrap'
      '';

      fh = ''
        history merge
        set command (history | fzf --query="$argv[1]" --height="40%" --layout="reverse" --no-multi)

        if test -n "$command"
          commandline --replace "$command"
        end
      '';
    };

    plugins = [
      {
        name = "autopair";
        src = pkgs.fishPlugins.autopair.src;
      }
    ];

    interactiveShellInit = ''
      set fish_greeting
      # Set VI keybindings
      # set -g fish_key_bindings fish_vi_key_bindings

      # Bind 'jk' to escape insert mode
      # bind -M insert jk 'set fish_bind_mode default; commandline -f repaint'

      # Configure fzf previews for files (bat) and directories (eza)
      set -x FZF_PREVIEW_COMMAND '
        if test -f {};
          bat --color=always --style=numbers --line-range :500 {};
        else;
          eza --tree --level=2 {};
        end'
    '';
  };
}
