    with import <nixpkgs> {}; {
      ExHack = stdenv.mkDerivation {
        name = "ExHack";
        buildInputs = [ haskell.compiler.ghc843 sqlite gnumake gcc-unwrapped cabal-install zlib ];
        NIX_LDFLAGS ="${zlib.out}/lib";
      };
    }

