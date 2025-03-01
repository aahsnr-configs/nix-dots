self: super: {
  stdenv = super.stdenvAdapters.addAttrsToDerivation {
    NIX_CFLAGS_COMPILE = "-march=native -O2 -pipe -fno-plt -fexceptions -Wp,-D_FORTIFY_SOURCE=3 -Wformat -Werror=format-security -fstack-clash-protection -fcf-protection -fno-omit-frame-pointer -mno-omit-leaf-frame-pointer";
  } super.stdenv;
}

