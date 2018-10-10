{ mkDerivation, base, bytestring, containers, directory, filepath
, ghcjs-dom, http-types, jsaddle, jsaddle-warp, lens, mtl, reflex
, reflex-dom-core, stdenv, text, wai, wai-middleware-static, warp
, websockets
}:
mkDerivation {
  pname = "exercises";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers directory filepath ghcjs-dom http-types
    jsaddle jsaddle-warp lens mtl reflex reflex-dom-core text wai
    wai-middleware-static warp websockets
  ];
  executableHaskellDepends = [ base reflex-dom-core ];
  license = stdenv.lib.licenses.bsd3;
}
