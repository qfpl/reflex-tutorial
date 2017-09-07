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
      ln -sf ${blog}/drafts $out/drafts

      mkdir -p $out/js/
      ln -sf ${code}/js/reflex $out/js/reflex

      mkdir -p $out/css
      ln -sf ${code}/css/reflex $out/css/reflex
    '';

    phases = ["installPhase"];
  }


