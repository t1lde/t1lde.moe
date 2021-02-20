{ mkDerivation, base, bifunctors, containers, fetchgit, filepath
, hpack, mtl, relude, split, stdenv, text, transformers
, unordered-containers
}:
mkDerivation {
  pname = "AOC2020";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/t1lde/AOC2020";
    sha256 = "0wdk9xqxz7lyawrhr6c7r8jym9xs06qda78cw27khsamka58zq20";
    rev = "9aaa0346234d4cf03043d2a8cd834e1102be3e02";
    fetchSubmodules = true;
  };
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bifunctors containers filepath mtl relude split text
    transformers unordered-containers
  ];
  prePatch = "hpack";
  license = stdenv.lib.licenses.bsd3;
}
