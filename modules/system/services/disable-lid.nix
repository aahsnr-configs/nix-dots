{ config, pkgs, ... }:

{
  systemd.user.services.disable-lid = {
    description = "Disable laptop lid on Hyprland startup and nixos-rebuild";

    # Provide the necessary packages in the service's PATH
    path = [ pkgs.hyprland pkgs.coreutils ];

    # The script that contains the logic for the two conditions
    script = ''
      set -e
      # Wait a moment for the Hyprland socket to be available
      sleep 1

      # Marker file to detect the first login after a reboot.
      # It's placed in the user's runtime directory, which is cleared on reboot.
      FIRST_LOGIN_MARKER="$XDG_RUNTIME_DIR/disable-lid-first-login-done"

      # Marker file to track the NixOS generation this was last run for.
      LAST_RUN_GENERATION_MARKER="$XDG_RUNTIME_DIR/disable-lid-last-generation"
      CURRENT_GENERATION_LABEL="${config.system.nixos.label}"

      run_hyprctl_command() {
        hyprctl keyword monitor 'eDP-1,disable'
      }

      # Case 1: First login after reboot. The first-login marker does not exist.
      if [ ! -f "$FIRST_LOGIN_MARKER" ]; then
        run_hyprctl_command
        touch "$FIRST_LOGIN_MARKER"
        echo "$CURRENT_GENERATION_LABEL" > "$LAST_RUN_GENERATION_MARKER"
        exit 0
      fi

      # Case 2: After `nixos-rebuild switch`. The system generation has changed.
      if [ -f "$LAST_RUN_GENERATION_MARKER" ] && \
         [ "$(cat "$LAST_RUN_GENERATION_MARKER")" != "$CURRENT_GENERATION_LABEL" ]; then
        run_hyprctl_command
        echo "$CURRENT_GENERATION_LABEL" > "$LAST_RUN_GENERATION_MARKER"
        exit 0
      fi
    '';

    # Start this service when the user logs into Hyprland
    wantedBy = [ "hyprland-session.target" ];

    # This is a one-shot service
    serviceConfig = {
      Type = "oneshot";
    };

    # This trick ensures that the service unit is considered "changed" after
    # every `nixos-rebuild switch`, which causes systemd to restart it.
    unitConfig.X-NixOS-GenerationLabel = "${config.system.nixos.label}";
  };
}
