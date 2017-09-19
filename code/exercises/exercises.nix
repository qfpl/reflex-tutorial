{ mkDerivation, base, containers, directory, filepath, ghcjs-dom
, jsaddle, jsaddle-warp, lens, mtl, reflex, reflex-dom-core, stdenv
, text, wai, wai-middleware-static, warp, websockets
}:
mkDerivation {
  pname = "exercises";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers directory filepath ghcjs-dom jsaddle jsaddle-warp
    lens mtl reflex reflex-dom-core text wai wai-middleware-static warp
    websockets
  ];
  license = stdenv.lib.licenses.bsd3;
}
