{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
in
  pkgs.stdenv.mkDerivation {
    name = "reflex-tutorial-blog";
    src = ./.;
    installPhase = ''
      mkdir -p $out
      cp -r $src/drafts $out/
    '';
    phases = ["installPhase"];
  }
