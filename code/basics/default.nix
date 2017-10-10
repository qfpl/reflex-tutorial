{ reflex-platform ? import ./reflex-platform.nix
, compiler   ? "ghcjs"
} :
let
  pkgs = reflex-platform.nixpkgs.pkgs;

  haskellPackages = reflex-platform.${compiler}.override {
    overrides = (self: super: {
      common = pkgs.haskell.lib.dontHaddock (import ../common { inherit compiler; });
      grid = pkgs.haskell.lib.dontHaddock (import ../grid { inherit compiler; });
    });
  };

  adjust-for-ghcjs = drv: {
    executableToolDepends = [pkgs.closurecompiler pkgs.zopfli];
    doHaddock = false;
    postInstall = ''
      mkdir -p $out

      mkdir -p $out/css/reflex/basics
      cp ./css/* $out/css/reflex/basics/

      mkdir -p $out/js/reflex/basics
      cp ./js/* $out/js/reflex/basics/
      cp $out/bin/reflex-basics.jsexe/all.js $out/js/reflex/basics/reflex-basics.js

      cd $out/bin/reflex-basics.jsexe
      closure-compiler all.js --compilation_level=ADVANCED_OPTIMIZATIONS --isolation_mode=IIFE --assume_function_wrapper --jscomp_off="*" --externs=all.js.externs > $out/js/reflex/basics/reflex-basics.min.js
      rm -Rf $out/bin

      cd $out/js/reflex/basics
      zopfli -i1000 reflex-basics.min.js

      rm -Rf $out/lib
      rm -Rf $out/nix-support
      rm -Rf $out/share
    '';
  };

  adjust = drv:
    if compiler == "ghcjs"
    then adjust-for-ghcjs drv
    else drv;

  basics = pkgs.haskell.lib.overrideCabal (haskellPackages.callPackage ./basics.nix {}) adjust;
in
  basics
