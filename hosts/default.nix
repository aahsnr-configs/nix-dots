{
  nixpkgs,
  self,
  catppuccin,
  chaotic,
  nixos-hardware,
  determinate,
  #gcc-overlay,
  ...
}:

let
  inputs = self.inputs;
  system = ../modules/system;
  laptop = nixos-hardware.nixosModules.asus-zephyrus-ga401;
  chaotic_nix = chaotic.homeManagerModules.default;
  pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
  hmModule = inputs.home-manager.nixosModules.home-manager;
  caTppuccin = catppuccin.nixosModules.catppuccin;
  deterMinate = determinate.nixosModules.default;

  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
    extraSpecialArgs = {
      inherit inputs;
      inherit self;
      inherit chaotic_nix;
    };
    users.ahsan = {
      imports = [
        (import ../modules/home)
        catppuccin.homeModules.catppuccin
      ];
    };
  };

in
{
  #workstation
  workstation = nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [
      { networking.hostName = "workstation"; }
      ./workstation/hardware-configuration.nix
      system
      hmModule
      caTppuccin
      { inherit home-manager; }
    ];
    specialArgs = { inherit inputs; };
  };

  #zephyrus
  zephyrus = nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [
      { networking.hostName = "zephyrus"; }
      ./zephyrus/hardware-configuration.nix
      system
      hmModule
      caTppuccin
      laptop
      deterMinate
      { inherit home-manager; }
      ##{ nixpkgs.overlays = [ gcc-overlay ]; }
    ];
    specialArgs = { inherit inputs; };
  };
}
