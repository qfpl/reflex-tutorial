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
      ln -sv ${blog}/drafts $out/drafts

      mkdir -p $out/js/
      ln -sv ${code}/js/reflex $out/js/reflex

      mkdir -p $out/css
      ln -sv ${code}/css/reflex $out/css/reflex
    '';

    phases = ["installPhase"];
  }


