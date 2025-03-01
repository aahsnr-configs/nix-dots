{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  name="dev-environment";
  buildInputs = with pkgs; [
    python3
    nodejs
  ];
  shellHook = ''
    echo "Starting developing.."
  '';

}

