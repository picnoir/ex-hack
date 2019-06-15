{ mkDerivation, base, stdenv, text, array, binary, bytestring, containers, deepseq, directory, filepath, ghc-prim, hpc, pretty, process, time, transformers, unix
}:
mkDerivation {
  pname = "ghc-lib-parser";
  version = "8.8.0.20190424";
  sha256 = "03f2racabmixc4jk3mn6k6cnhapaplswa8fbb9yajrzj56ag16wx";
  libraryHaskellDepends = [
  array base binary bytestring containers deepseq directory filepath ghc-prim hpc pretty process time transformers unix
  ];
  license = stdenv.lib.licenses.mit;
}
