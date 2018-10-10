{ reflex-platform ? import ../reflex-platform.nix
, compiler   ? "ghcjs"
} :
let
  pkgs = reflex-platform.nixpkgs.pkgs;
  
  haskellPackages = reflex-platform.${compiler}.override {
    overrides = self: super: {
      wai-middleware-static = pkgs.haskell.lib.dontCheck (super.wai-middleware-static); 
      http-date = pkgs.haskell.lib.dontCheck super.http-date;
      iproute = pkgs.haskell.lib.dontCheck super.iproute;
      Glob = pkgs.haskell.lib.dontCheck super.Glob;
      http2 = pkgs.haskell.lib.dontCheck super.http2;
      bsb-http-chunked = pkgs.haskell.lib.dontCheck super.bsb-http-chunked;
      SHA = pkgs.haskell.lib.dontCheck super.SHA;
      tasty = pkgs.haskell.lib.doJailbreak super.tasty;
    }; 
  };

  adjust-for-ghcjs = drv: {
    executableToolDepends = [pkgs.closurecompiler pkgs.zopfli];
    doHaddock = false;
    postInstall = ''
      mkdir -p $out

      mkdir -p $out/css/reflex/basics-exercises
      cp -r ./css/exercises $out/css/reflex/basics-exercises/
      cp -r ./css/solutions $out/css/reflex/basics-exercises/

      mkdir -p $out/js/reflex/basics-exercises
      cp $out/bin/solutions.jsexe/all.js $out/js/reflex/basics-exercises/solutions.js

      cd $out/bin/solutions.jsexe
      closure-compiler all.js --compilation_level=ADVANCED_OPTIMIZATIONS --isolation_mode=IIFE --assume_function_wrapper --jscomp_off="*" --externs=all.js.externs > $out/js/reflex/basics-exercises/solutions.min.js
      rm -Rf $out/bin/solutions.jsexe
      rm -Rf $out/bin

      cd $out/js/reflex/basics-exercises
      zopfli -i1000 solutions.min.js

      rm -Rf $out/lib
      rm -Rf $out/nix-support
      rm -Rf $out/share
    '';
  };

  adjust = drv:
    if compiler == "ghcjs"
    then adjust-for-ghcjs drv
    else drv;

  exercises = pkgs.haskell.lib.overrideCabal (haskellPackages.callPackage ./exercises.nix {}) adjust;
in
  exercises
