{pkgs, ...}: let
  tex = pkgs.texlive.combine {
    inherit
      (pkgs.texlive)
      scheme-medium
      metapost
      xetex
      dvisvgm
      dvipng
      wrapfig
      amsmath
      ulem
      hyperref
      capt-of
      physics
      siunitx
      booktabs
      latex
      ;
  };
in {
  home.packages = with pkgs; [
    tex
    texlab
    tectonic
    ghostscript
    imagemagick
  ];
}
