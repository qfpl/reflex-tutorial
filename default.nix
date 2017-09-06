let
  pkgs = import <nixpkgs> {};

  blog = import ./blog {};
  code = import ./code/basics {};
in
  pkgs.stdenv.mkDerivation rec {
    name = "reflex-tutorial";
    src = ./.;

    installPhase = ''
      mkdir -p $out
      cp -r ${blog}/* $out/

      mkdir -p $out/js/
      cp -r ${code}/js/* $out/js/

      mkdir -p $out/css
      cp -r ${code}/css/* $out/css/
    '';

    phases = ["installPhase"];
  }


