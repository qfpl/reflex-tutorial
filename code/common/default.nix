{ reflex-platform ? import ../reflex-platform.nix
, compiler   ? "ghcjs"
} :
let
  pkgs = reflex-platform.nixpkgs.pkgs;
  haskellPackages = reflex-platform.${compiler}.override {
    overrides = self: super: {
      http-date = pkgs.haskell.lib.dontCheck super.http-date;
      iproute = pkgs.haskell.lib.dontCheck super.iproute;
      Glob = pkgs.haskell.lib.dontCheck super.Glob;
      http2 = pkgs.haskell.lib.dontCheck super.http2;
      bsb-http-chunked = pkgs.haskell.lib.dontCheck super.bsb-http-chunked;
      SHA = pkgs.haskell.lib.dontCheck super.SHA;
    };
  };
  common = haskellPackages.callPackage ./common.nix {};
in
  common
