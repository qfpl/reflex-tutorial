{ reflex-platform ? import ../reflex-platform.nix
, compiler   ? "ghcjs"
} :
let
  pkgs = reflex-platform.nixpkgs.pkgs;
  common = reflex-platform.${compiler}.callPackage ./common.nix {};
in
  common
