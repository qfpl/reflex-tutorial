{ mkDerivation, base, common, containers, data-default, ghcjs-dom
, jsaddle, lens, mtl, reflex, reflex-dom-core, stdenv, text
}:
mkDerivation {
  pname = "grid";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base common containers data-default ghcjs-dom jsaddle lens mtl
    reflex reflex-dom-core text
  ];
  license = stdenv.lib.licenses.bsd3;
}
