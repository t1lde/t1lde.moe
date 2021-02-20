{ mkDerivation, aeson, base, bifunctors, binary, bytestring
, deriving-aeson, exceptions, filepath, hakyll, hpack, http-client
, http-client-tls, http-types, mtl, pandoc, relude, stdenv, text
, time
}:
mkDerivation {
  pname = "HakyllSite";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bifunctors binary bytestring deriving-aeson exceptions
    filepath hakyll http-client http-client-tls http-types mtl pandoc
    relude text time
  ];
  prePatch = "hpack";
  description = "Personal Site";
  license = stdenv.lib.licenses.bsd3;
}
