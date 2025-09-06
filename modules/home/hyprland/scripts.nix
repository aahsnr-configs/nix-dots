# scripts.nix
# This module packages custom Python scripts for Hyprland and adds them to the
# user's environment.
{pkgs, ...}: let
  ##----------------------------------------------------------------##
  ##-------------------------- wsaction ----------------------------##
  ##----------------------------------------------------------------##
  # Packages the wsaction Python script into an executable file.
  # pkgs.writeScriptBin automatically handles patching the shebang to point to the
  # correct Python 3 interpreter provided by Nix.
  wsaction-script = pkgs.writeScriptBin "wsaction" ''
    #!/usr/bin/env python3

    """
    A script to dispatch workspace actions in Hyprland, supporting both individual
    workspace and group-based navigation.
    """

    import argparse
    import json
    import subprocess
    import sys
    from typing import NoReturn

    WORKSPACES_PER_GROUP = 10

    def get_active_workspace_id() -> int:
        """Retrieves the ID of the currently active workspace in Hyprland."""
        try:
            result = subprocess.run(
                ["hyprctl", "activeworkspace", "-j"],
                capture_output=True, text=True, check=True, encoding="utf-8"
            )
            return int(json.loads(result.stdout)["id"])
        except FileNotFoundError:
            print("Error: 'hyprctl' command not found. Is Hyprland running?", file=sys.stderr)
            sys.exit(1)
        except subprocess.CalledProcessError as e:
            print(f"Error executing 'hyprctl': {e}\nStderr: {e.stderr.strip()}", file=sys.stderr)
            sys.exit(1)
        except (json.JSONDecodeError, KeyError, TypeError):
            print("Error: Failed to parse valid JSON from 'hyprctl'.", file=sys.stderr)
            sys.exit(1)

    def create_parser() -> argparse.ArgumentParser:
        """Creates and configures the argument parser for the script."""
        parser = argparse.ArgumentParser(description="A script for workspace actions in Hyprland.")
        parser.add_argument("-g", "--group", action="store_true", help="Perform a group action.")
        parser.add_argument("dispatcher", type=str, help="The hyprctl dispatcher (e.g., 'workspace').")
        parser.add_argument("target", type=int, help="The target workspace or group number.")
        return parser

    def main() -> NoReturn:
        """Parses arguments and executes the Hyprland dispatch command."""
        parser = create_parser()
        args = parser.parse_args()

        try:
            active_ws = get_active_workspace_id()
            target_ws: int

            if args.group:
                current_ws_index = ((active_ws - 1) % WORKSPACES_PER_GROUP) + 1
                target_group_base = (args.target - 1) * WORKSPACES_PER_GROUP
                target_ws = target_group_base + current_ws_index
            else:
                current_group_base = (active_ws - 1) // WORKSPACES_PER_GROUP * WORKSPACES_PER_GROUP
                target_ws = current_group_base + args.target

            subprocess.run(
                ["hyprctl", "dispatch", args.dispatcher, str(target_ws)],
                check=True,
            )
        except Exception as e:
            print(f"An unexpected error occurred: {e}", file=sys.stderr)
            sys.exit(1)

        # A call to sys.exit() is required at the end of a successful path
        # to satisfy the `NoReturn` type hint.
        sys.exit(0)

    if __name__ == "__main__":
        main()
  '';

  ##----------------------------------------------------------------##
  ##------------------------ disable-lid ---------------------------##
  ##----------------------------------------------------------------##

  # Packages the disable-lid Python script into an executable file.
  disable-lid-script = pkgs.writeScriptBin "disable-lid" ''
    #!/usr/bin/env python3

    """
    A script to disable the eDP-1 monitor using Hyprland's dispatcher.
    Intended for use with a laptop lid-closing event.
    """

    import subprocess
    import sys
    from typing import NoReturn

    def main() -> NoReturn:
        """Executes the hyprctl command to disable the eDP-1 monitor."""
        try:
            command = ["hyprctl", "keyword", "monitor", "eDP-1, disable"]
            result = subprocess.run(
                command, capture_output=True, text=True, encoding="utf-8"
            )

            if result.returncode != 0 and "ok" not in result.stdout.lower():
                print("Error executing 'hyprctl keyword monitor'", file=sys.stderr)
                print(f"Return Code: {result.returncode}", file=sys.stderr)
                print(f"Stderr: {result.stderr.strip()}", file=sys.stderr)
                sys.exit(1)

        except FileNotFoundError:
            print("Error: 'hyprctl' command not found. Is Hyprland running?", file=sys.stderr)
            sys.exit(1)
        except Exception as e:
            print(f"An unexpected error occurred: {e}", file=sys.stderr)
            sys.exit(1)

        # A call to sys.exit() is required at the end of a successful path
        # to satisfy the `NoReturn` type hint.
        sys.exit(0)

    if __name__ == "__main__":
        main()
  '';
in {
  # Add both script packages to the list of packages to be installed in the
  # user's environment, making them available in the PATH.
  home.packages = [wsaction-script disable-lid-script];
}
