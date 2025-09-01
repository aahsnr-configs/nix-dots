# wsaction.nix
# This module packages the Python workspace script and adds it to the user's PATH.

{ pkgs, ... }:

let
  # Package the python script into an executable file named "wsaction".
  # Nix will place this script in the Nix store and symlink it into your profile.
  # The shebang `#!/usr/bin/env python3` will be automatically patched by Nix
  # to point to the correct python3 interpreter from your nixpkgs.
  wsaction-script = pkgs.writeScriptBin "wsaction" ''
    #!/usr/bin/env python3

    """
    A script to dispatch workspace actions in Hyprland.

    This script can move to a workspace within the current group or move to a
    different group of workspaces entirely. It corrects a subtle logic error from
    the original fish script related to workspaces that are multiples of 10.
    """

    import argparse
    import json
    import math
    import subprocess
    import sys
    from typing import NoReturn

    # Define the number of workspaces per group for clarity and easy modification.
    WORKSPACES_PER_GROUP = 10


    def get_active_workspace_id() -> int:
        """
        Retrieves the ID of the currently active workspace in Hyprland.

        Returns:
            The integer ID of the active workspace.

        Raises:
            SystemExit: If the 'hyprctl' command fails or returns invalid JSON.
        """
        try:
            result = subprocess.run(
                ["hyprctl", "activeworkspace", "-j"],
                capture_output=True,
                text=True,
                check=True,  # Raise an exception for non-zero exit codes
                encoding="utf-8",
            )
            workspace_info = json.loads(result.stdout)
            return int(workspace_info["id"])
        except FileNotFoundError:
            print("Error: 'hyprctl' command not found.", file=sys.stderr)
            print("Please ensure Hyprland is running and 'hyprctl' is in your PATH.", file=sys.stderr)
            sys.exit(1)
        except subprocess.CalledProcessError as e:
            print(f"Error executing 'hyprctl': {e}", file=sys.stderr)
            print(f"Stderr: {e.stderr.strip()}", file=sys.stderr)
            sys.exit(1)
        except (json.JSONDecodeError, KeyError, TypeError):
            print("Error: Failed to parse JSON or find 'id' from 'hyprctl'.", file=sys.stderr)
            sys.exit(1)


    def create_parser() -> argparse.ArgumentParser:
        """Creates and configures the argument parser for the script."""
        parser = argparse.ArgumentParser(
            description="A script to dispatch workspace or group actions in Hyprland.",
            epilog="Example: %(prog)s movetoworkspace 3 -> moves to workspace 3 in the current group."
        )
        parser.add_argument(
            "-g", "--group",
            action="store_true",
            help="Perform a group action instead of a workspace action."
        )
        parser.add_argument(
            "dispatcher",
            type=str,
            help="The hyprctl dispatcher to use (e.g., 'workspace', 'movetoworkspace')."
        )
        parser.add_argument(
            "target",
            type=int,
            help="The target workspace or group number (an integer)."
        )
        return parser


    def main() -> NoReturn:
        """
        Parses arguments and executes the Hyprland dispatch command.
        """
        parser = create_parser()
        args = parser.parse_args()

        try:
            active_ws = get_active_workspace_id()
            target_ws: int

            if args.group:
                # Logic for moving to a different group.
                # This logic correctly handles workspaces that are multiples of 10.
                current_ws_index = ((active_ws - 1) % WORKSPACES_PER_GROUP) + 1
                target_group_base = (args.target - 1) * WORKSPACES_PER_GROUP
                target_ws = target_group_base + current_ws_index
            else:
                # Logic for moving to a workspace within the current group.
                current_group_base = (
                    math.floor((active_ws - 1) / WORKSPACES_PER_GROUP) * WORKSPACES_PER_GROUP
                )
                target_ws = current_group_base + args.target

            # Execute the final command.
            subprocess.run(
                ["hyprctl", "dispatch", args.dispatcher, str(target_ws)],
                check=True,
            )

        except subprocess.CalledProcessError as e:
            print(f"Error dispatching to Hyprland: {e}", file=sys.stderr)
            sys.exit(1)
        except Exception as e:
            print(f"An unexpected error occurred: {e}", file=sys.stderr)
            sys.exit(1)

        sys.exit(0)


    if __name__ == "__main__":
        main()
  '';
in
{
  # Add the script package to the list of packages to be installed in the
  # user's environment.
  home.packages = [ wsaction-script ];
}
