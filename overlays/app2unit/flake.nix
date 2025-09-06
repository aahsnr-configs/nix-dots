{
  description = "A flake overlay for the app2unit package";

  inputs = {
    # Pin to a specific nixpkgs revision for reproducibility.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Fetch the app2unit source code from its GitHub repository.
    app2unit-src = {
      url = "github:Vladimir-csp/app2unit";
      # This repository is not a flake, so we must set this to false.
      flake = false;
    };
  };

  outputs = {
    self,
    nixpkgs,
    app2unit-src,
  }: let
    # A set of common systems to generate outputs for.
    supportedSystems = ["x86_64-linux" "aarch64-linux"];

    # Helper function to generate attributes for each supported system.
    forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
  in {
    # The primary output: an overlay that adds the app2unit package.
    overlays.default = final: prev: {
      app2unit = prev.stdenv.mkDerivation rec {
        pname = "app2unit";
        # Create a version string from the source's last modified date and git revision.
        version = "unstable-${builtins.substring 0 8 app2unit-src.lastModifiedDate}-${
          builtins.substring 0 7 app2unit-src.rev
        }";

        src = app2unit-src;

        # The source is a single script, so no build step is needed.
        dontBuild = true;

        # The installation phase copies the script to the output directory.
        installPhase = ''
          runHook preInstall
          install -Dm755 $src/app2unit $out/bin/app2unit
          runHook postInstall
        '';

        meta = with prev.lib; {
          description = "Application launcher, file opener, and terminal launcher for systemd environments";
          homepage = "https://github.com/Vladimir-csp/app2unit";
          license = licenses.mit;
          platforms =
            platforms.linux; # This script is specific to Linux with systemd.
        };
      };
    };

    # Expose the package directly for easy testing (e.g., with `nix build .`).
    # This part has been corrected to properly apply the overlay.
    packages = forAllSystems (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {default = (pkgs.extend self.overlays.default).app2unit;});
  };
}
