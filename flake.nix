{
  description = "NixOS configuration of Ahsanur Rahman";

  inputs = {
    anyrun = {
      url = "github:anyrun-org/anyrun";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    app2unit = {
      url = "github:Vladimir-csp/app2unit";
      flake = false;
    };

    catppuccin.url = "github:catppuccin/nix";

    chaotic.url = "github:chaotic-cx/nyx/nyxpkgs-unstable";

    devshell.url = "github:numtide/devshell";

    dgop = {
      url = "github:AvengeMedia/dgop";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    dms-cli = {
      url = "github:AvengeMedia/danklinux";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    dankMaterialShell = {
      url = "github:AvengeMedia/DankMaterialShell";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.dgop.follows = "dgop";
      inputs.dms-cli.follows = "dms-cli";
    };

    dsearch = {
      url = "github:AvengeMedia/danksearch";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-doom-emacs-unstraightened = {
      url = "github:marienz/nix-doom-emacs-unstraightened";
      inputs.nixpkgs.follows = "";
    };

    nix-gaming.url = "github:fufexan/nix-gaming";

    nixos-hardware.url = "github:nixos/nixos-hardware";

    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    nixpkgs-wayland = {
      url = "github:nix-community/nixpkgs-wayland";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-vscode-extensions = {
      url = "github:nix-community/nix-vscode-extensions";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nvf = {
      url = "github:NotAShelf/nvf";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pyprland = {
      url = "github:hyprland-community/pyprland";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    quickshell = {
      url = "git+https://git.outfoxxed.me/outfoxxed/quickshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    schizofox.url = "github:schizofox/schizofox";

    systems.url = "github:nix-systems/default-linux";

    yazi = {
      url = "github:sxyazi/yazi";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    zen-browser = {
      url = "github:0xc000022070/zen-browser-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      home-manager,
      emacs-overlay,
      nixpkgs,
      yazi,
      nix-doom-emacs-unstraightened,
      pyprland,
      anyrun,
      chaotic,
      nvf,
      ...
    }@inputs:
    let
      system = "x86_64-linux";
    in
    {
      nixosConfigurations = import ./hosts inputs;

      packages.${system} = {
        #catppuccin-folders = pkgs.callPackage ./pkgs/catppuccin-folders.nix {};
        #catppuccin-gtk = pkgs.callPackage ./pkgs/catppuccin-gtk.nix {};
        #catppuccin-cursors = pkgs.callPackage ./pkgs/catppuccin-cursors.nix {};
        #onlyoffice-deb = pkgs.callPackage ./pkgs/onlyoffice-bin.nix {};
        #insync-deb = pkgs.callPackage ./pkgs/insync-deb.nix {};
      };
    };
}
