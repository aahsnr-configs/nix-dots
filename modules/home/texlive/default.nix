{pkgs, ...}: let
  tex = pkgs.texlive.combine {
    inherit
      (pkgs.texlive)
      scheme-minimal
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
