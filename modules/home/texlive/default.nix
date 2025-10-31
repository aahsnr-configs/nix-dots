{
  config,
  pkgs,
  ...
}: let
  tex = pkgs.texlive.combine {
    inherit
      (pkgs.texlive)
      scheme-medium
      dvisvgm
      dvipng
      wrapfig
      amsmath
      ulem
      hyperref
      capt-of
      physics
      siunitx
      ;
  };
in {
  # home-manager
  home.packages = with pkgs; [
    tex
    tectonic
    texlab
    ghostscript
    imagemagick
  ];
}
