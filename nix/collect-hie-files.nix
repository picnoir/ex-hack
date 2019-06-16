{
  pkg-name  ? "colour",
  compiler  ? "ghc881",
  rev       ? "8c14a6f641b7f3baa57e55e784a0d8626325446b",
  sha256    ? "0jbkkjqpjjr63agh7p8pbnlyq2kknl9a1wbrxhaf8033j9f9fni7",
  config    ? {}
}:

let
  pkgs = import (builtins.fetchTarball {
     url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
     inherit sha256; }) {inherit config;};
  haskellLib = pkgs.haskell.lib;
  inherit (pkgs) stdenv all-cabal-hashes;
  justHieInterfaceFile = drv : haskellLib.overrideCabal drv (drv :{
    isLibrary = true;
    doHaddock = false;
    enableLibraryProfiling = false;
    configureFlags = [ "--ghc-option=-fwrite-ide-info" ];
    postBuild = ''
      mkdir -p $out/hie
      pushd ./dist/build
      find . -name '*.hie' | ${pkgs.cpio}/bin/cpio -pdm $out/hie
      popd
    '';
  });
in
 justHieInterfaceFile pkgs.haskell.packages.${compiler}.${pkg-name}
