with import <nixpkgs> {}; {
  ExHack = stdenv.mkDerivation {
    name = "ExHack";
    buildInputs = [ sqlite stack ];
    NIX_LDFLAGS ="${sqlite.out}/lib";
  };
}
