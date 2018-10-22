{ mkDerivation, ansi-terminal, base, blaze-html, bytestring, Cabal
, cabal-helper, cabal-install, containers, deepseq, directory, exceptions
, file-embed, filepath, ghc, ghc-paths, hashable, hspec
, http-client, http-client-tls, lens, mtl, network-uri
, optparse-applicative, process, pygments, safe, selda, selda-sqlite
, shakespeare, stdenv, tar, text, unordered-containers, yaml, zlib
}:
mkDerivation {
  pname = "ex-hack";
  version = "0.1.0.0";
  src = ../.;
  isLibrary = true;
  isExecutable = true;
  doCheck = false;
  libraryHaskellDepends = [
    ansi-terminal base blaze-html bytestring Cabal cabal-helper
    containers deepseq directory exceptions file-embed filepath ghc
    ghc-paths hashable http-client http-client-tls lens mtl network-uri
    process safe selda selda-sqlite shakespeare tar text
    unordered-containers yaml zlib
  ];
  executableHaskellDepends = [
    base directory filepath lens optparse-applicative text
  ];
  testHaskellDepends = [
    base containers directory file-embed filepath hspec text
  ];
  homepage = "https://github.com/TORELEASE";
  license = stdenv.lib.licenses.gpl3;
  # We need to rewrite the runtime binary dependencies to their correct nix path.
  postConfigure = ''
          substituteInPlace src/ExHack/Cabal/Cabal.hs --replace 'cabalPath = "cabal"' 'cabalPath = "${cabal-install}/bin/cabal"'
          substituteInPlace src/ExHack/Renderer/Html.hs --replace '"pygmentize"' '"${pygments}/bin/pygmentize"'
        '';
}
