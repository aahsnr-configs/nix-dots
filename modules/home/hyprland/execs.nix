{...}: {
  wayland.windowManager.hyprland = {
    settings = {
      exec-once = [
        "pypr"
        "mpris-proxy"
        "foot --server"
        "emacs --daemon"
        # "dms run"
        "caelestia resizer -d"
        "caelestia shell -d"
        "trash-empty 30"
        "systemctl --user start hyprpolkitagent"
      ];
    };
    extraConfig = ''
      exec = cp -L --no-preserve=mode --update=none $HOME/.config/hypr/scheme/default.conf $HOME/.config/hypr/scheme/current.conf
      source = $HOME/.config/hypr/scheme/current.conf

      # exec = touch -a /home/ahsan/.config/caelestia/hypr-vars.conf
      # source = $HOME/.config/caelestia/hypr-vars.conf
      #
      # exec = touch -a $HOME/.config/caelestia/hypr-user.conf
      # source = $HOME/.config/caelestia/hypr-user.conf
    '';
  };
}
