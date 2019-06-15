{ mkDerivation, base, stdenv, text, array, binary, bytestring, containers, deepseq, directory
}:
mkDerivation {
  pname = "ghc-lib";
  version = "8.8.0.20190424";
  sha256 = "03f1racabmixc4jk3mn6k6cnhapaplswa8fbb9yajrzj56ag16wm";
  libraryHaskellDepends = [
  array base binary bytestring containers deepseq directory
  ];
  license = stdenv.lib.licenses.mit;
}
