{ mkDerivation, base, bytestring, Cabal, cabal-install, cabal-plan
, containers, directory, filepath, ghc, ghc-paths, mtl, pretty-show
, process, semigroupoids, stdenv, template-haskell, temporary, text
, transformers, unix, unix-compat, utf8-string
}:
mkDerivation {
  pname = "cabal-helper";
  version = "0.8.1.2";
  sha256 = "345ad343095f75c1223f26052fa7c1966397427318594baafb7ea62a825abedf";
  isLibrary = true;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal ];
  libraryHaskellDepends = [
    base Cabal cabal-plan containers directory filepath mtl process
    semigroupoids transformers unix unix-compat
  ];
  executableHaskellDepends = [
    base bytestring Cabal cabal-plan containers directory filepath mtl
    pretty-show process template-haskell temporary text transformers
    unix unix-compat utf8-string
  ];
  executableToolDepends = [ cabal-install ];
  testHaskellDepends = [
    base bytestring Cabal cabal-plan containers directory filepath ghc
    ghc-paths mtl pretty-show process template-haskell temporary text
    transformers unix unix-compat utf8-string
  ];
  testToolDepends = [ cabal-install ];
  doCheck = false;
  description = "Simple interface to some of Cabal's configuration state, mainly used by ghc-mod";
  license = stdenv.lib.licenses.gpl3;
}
