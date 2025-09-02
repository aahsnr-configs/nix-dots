{ nixpkgs, ... }:
{
  nixpkgs.overlays = [
    (self: super: {
      stdenv = super.overrideCC super.stdenv self.gcc15;
    })
  ];
}
