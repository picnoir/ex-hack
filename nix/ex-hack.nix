{ mkDerivation, ansi-terminal, base, blaze-html, bytestring, Cabal
, cabal-helper, containers, deepseq, directory, exceptions
, file-embed, filepath, ghc, ghc-paths, hashable, hspec
, http-client, http-client-tls, lens, mtl, network-uri
, optparse-applicative, process, pygments, safe, selda, selda-sqlite
, shakespeare, stdenv, tar, text, unordered-containers, yaml, zlib, stack
}:
mkDerivation rec {
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

  # Dirty hack: cabal-helper seems to dislike nix-build and makes the whole
  # build to fail if doCheck is enabled. However, we still want to have
  # the test deps to run cabal test in the CI Script. With doCheck disabled,
  # we won't get the test deps in scope, so instead, we force them as build deps.
  buildDepends = testHaskellDepends;
  executableHaskellDepends = [
    base directory filepath lens optparse-applicative text
  ];
  testHaskellDepends = [
    base containers directory file-embed filepath hspec text
  ];
  homepage = "https://github.com/TORELEASE";
  license = stdenv.lib.licenses.gpl3;
  # We need to rewrite the runtime binary dependencies to their correct nix path.
  #
  # We also need to update cabal-install local database. If we don't do this, cabal-helper will 
  # fail building its helper binary and this will make the integration tests fail.
  postConfigure = ''
          substituteInPlace src/ExHack/Cabal/Cabal.hs --replace 'cabalPath = "cabal"' 'cabalPath = "${stack}/bin/stack"'
          substituteInPlace src/ExHack/Renderer/Html.hs --replace '"pygmentize"' '"${pygments}/bin/pygmentize"'
        '';
}
