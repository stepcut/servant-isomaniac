{ mkDerivation, bytestring, exceptions, ghcjs-base, hsp, hsx2hs
, http-api-data, http-media, http-types, isomaniac, safe, servant
, stdenv, stm, text, transformers
}:
mkDerivation {
  pname = "servant-isomaniac";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    bytestring exceptions ghcjs-base hsp hsx2hs http-api-data
    http-media http-types isomaniac safe servant stm text transformers
  ];
  license = stdenv.lib.licenses.bsd3;
}
