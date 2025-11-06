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
      latex
      ;
  };
in {
  home.packages = with pkgs; [
    texliveFull
    texlab
    tectonic
    ghostscript
    imagemagick
  ];
}
