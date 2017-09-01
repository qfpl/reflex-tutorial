let
  drv = import ./default.nix { compiler = "ghc"; };
in
  drv.env
