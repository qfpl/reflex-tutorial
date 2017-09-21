{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc"
}:
let
  reflex-platform = import ./reflex-platform.nix;
  pkgs = reflex-platform.nixpkgs.pkgs;
  drv = import ./. { inherit reflex-platform compiler; };
  drvWithTools = pkgs.haskell.lib.addBuildDepends drv 
    [ pkgs.cabal-install 
      reflex-platform.ghc.ghcid
    ];
in
  drvWithTools.env
