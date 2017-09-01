{ mkDerivation, base, containers, directory, filepath, ghcjs-dom
, jsaddle, jsaddle-warp, reflex-dom-core, stdenv, text, wai
, wai-middleware-static, warp, websockets
}:
mkDerivation {
  pname = "common";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers directory filepath ghcjs-dom jsaddle jsaddle-warp
    reflex-dom-core text wai wai-middleware-static warp websockets
  ];
  license = stdenv.lib.licenses.bsd3;
}
