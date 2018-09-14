# TODO pin nixpkgs to avoid this mess.
with import <nixpkgs> {}; rec {
  unstable = import <nixos-unstable> {};
  ExHack = stdenv.mkDerivation {
    name = "ExHack";
    buildInputs = [ unstable.stack haskell.compiler.ghc843 sqlite gnumake gcc-unwrapped cabal-install zlib ];
  };
}

