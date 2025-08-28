{ ... }: {
  programs = {
    less.enable = true;

    zsh = {
      enable = true;
      enableCompletion = true;
      autosuggestions.enable = true;
      syntaxHighlighting.enable = true;
      # interactiveShellInit = ''
      # # Enable the below for profiling zsh's startup speed.
      # # Once enabled, get numbers using:
      # #     zsh -i -l -c 'zprof'
      # #zmodload zsh/zprof
      #
      # # Disable `compaudit` being invoked from GRML cominit call.
      # # See: https://grml.org/zsh/grmlzshrc.html
      # # This speeds up shell loading.
      # zstyle ':grml:completion:compinit' arguments -C
      #
      # # Load grml's zshrc.
      # # Note that loading grml's zshrc here will override NixOS settings such as
      # # `programs.zsh.histSize`, so they will have to be set again below.
      # source ${pkgs.grml-zsh-config}/etc/zsh/zshrc
      #
      # # From https://htr3n.github.io/2018/07/faster-zsh/
      # # Theoretically it should not be needed (as described on https://dev.to/djmoch/zsh-compinit--rtfm-47kg)
      # # but I couldn't figure out how to make the GRML zshrc do only a single compinit
      # # without compaudit but generating .zcompdump (I use `-C` for
      # # `:grml:completion:compinit` above to avoid compaudit but that also skips
      # # generating `.zcompdump` apparently).
      # # Snippet based on https://gist.github.com/ctechols/ca1035271ad134841284
      # autoload -Uz compinit
      # if [[ -n ${ZDOTDIR:-$HOME}/.zcompdump(#qN.mh+24) ]]; then
      #   compinit
      # else
      #   # We don't do `compinit -C` here because the GRML zshrc already did it above.
      # fi
      # '';
      promptInit = "";
      enableGlobalCompInit = true;
    };
  };

  # users.defaultUserShell = pkgs.zsh;
}
