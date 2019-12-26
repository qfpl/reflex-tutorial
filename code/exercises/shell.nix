{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc"
}:
let
  reflex-platform = import ../reflex-platform.nix;
  pkgs = reflex-platform.nixpkgs.pkgs;
  drv = import ./. { inherit reflex-platform compiler; };
  drvWithTools = 
    [ pkgs.cabal-install 
      reflex-platform.ghc.ghcid
    ];
  shellDrv =
    pkgs.haskell.lib.overrideCabal
      drv
      (drv': {
        buildDepends = (drv'.buildDepends or []) ++
          [ (pkgs.haskellPackages.hoogleLocal {
              packages =
                (drv'.libraryHaskellDepends or []) ++
                (drv'.executableHaskellDepends or [])++
                (drv'.testHaskellDepends or []);
              })
          ] ++
          drvWithTools;
});
in
  shellDrv.env
