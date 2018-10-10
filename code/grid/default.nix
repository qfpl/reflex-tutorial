{ reflex-platform ? import ../reflex-platform.nix
, compiler   ? "ghcjs"
} :
let
  pkgs = reflex-platform.nixpkgs.pkgs;

  haskellPackages = reflex-platform.${compiler}.override {
    overrides = (self: super: rec {
      common = pkgs.haskell.lib.dontHaddock (import ../common { inherit compiler; });
      tasty = pkgs.haskell.lib.doJailbreak super.tasty;
    });
  };

  grid = haskellPackages.callPackage ./grid.nix {};
in
  grid
