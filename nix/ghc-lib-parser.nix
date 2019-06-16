{ mkDerivation, base, stdenv, text, array, binary, bytestring, containers, deepseq, directory, filepath, ghc-prim, hpc, pretty, process, time, transformers, unix
}:
mkDerivation {
  pname = "ghc-lib-parser";
  version = "8.8.0.20190424";
  sha256 = "12gsh994pr13bsybwlravmi21la66dyw74pk74yfw2pnz682wv10";
  libraryHaskellDepends = [
  array base binary bytestring containers deepseq directory filepath ghc-prim hpc pretty process time transformers unix
  ];
  license = stdenv.lib.licenses.mit;
}
