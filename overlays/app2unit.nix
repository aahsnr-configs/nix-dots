# ./overlays/app2unit.nix
# This overlay adds our custom app2unit package to the pkgs set
{ inputs, ... }:
final: prev: {
  app2unit = prev.callPackage ../pkgs/app2unit.nix {
    # Pass the flake input source to the package derivation
    app2unit-src = inputs.app2unit;
  };
}
