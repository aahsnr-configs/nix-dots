{ stdenv, lib, scdoc, libnotify, dash, makeWrapper, app2unit-src, }:
stdenv.mkDerivation {
  pname = "app2unit-git";
  version = app2unit-src.shortRev or "dirty";

  src = app2unit-src;

  # scdoc is for building man pages; makeWrapper is for fixing runtime paths
  nativeBuildInputs = [ scdoc makeWrapper ];

  # dash is the shell; libnotify provides `notify-send`
  buildInputs = [ dash libnotify ];

  makeFlags = [ "PREFIX=${placeholder "out"}" ];

  # This new phase runs after the default `make install`
  postInstall = ''
    # Patch the shebang to point to the `dash` binary in the Nix store
    substituteInPlace $out/bin/app2unit \
      --replace "/bin/sh" "${lib.getBin dash}/bin/sh"

    # Wrap the executable to ensure `notify-send` is always in its PATH
    wrapProgram $out/bin/app2unit \
      --prefix PATH : "${lib.getBin libnotify}/bin"
  '';

  meta = with lib; {
    description = "Launches Desktop Entries as Systemd user units";
    homepage = "https://github.com/Vladimir-csp/app2unit";
    license = licenses.gpl3Only;
    maintainers = with maintainers; [ aahsnr ];
    platforms = platforms.linux;
  };
}
