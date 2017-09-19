let
  pkgs = import <nixpkgs> {};

  blog = import ./blog {};
  code = import ./code/basics {};
  exercises = import ./code/exercises {};
in
  pkgs.stdenv.mkDerivation rec {
    name = "reflex-tutorial";
    src = ./.;

    installPhase = ''
      mkdir -p $out
      ln -sv ${blog}/drafts $out/drafts
      ln -sv ${blog}/posts $out/posts

      mkdir -p $out/js/reflex
      ln -sv ${code}/js/reflex/basics $out/js/reflex/basics
      ln -sv ${exercises}/js/reflex/basics-exercises $out/js/reflex/basics-exercises

      mkdir -p $out/css/reflex
      ln -sv ${code}/css/reflex/basics $out/css/reflex/basics
      ln -sv ${exercises}/css/reflex/basics-exercises $out/css/reflex/basics-exercises
    '';

    phases = ["installPhase"];
  }


