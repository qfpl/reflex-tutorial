{ mkDerivation, base, common, containers, data-default, ghcjs-dom
, lens, mtl, reflex, reflex-dom-core, stdenv, text, time
}:
mkDerivation {
  pname = "basics";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base common containers data-default ghcjs-dom lens mtl reflex
    reflex-dom-core text time
  ];
  license = stdenv.lib.licenses.bsd3;
}
